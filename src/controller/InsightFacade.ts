import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
} from "./IInsightFacade";
import {
	checkValidId,
	extractFileStrings,
	getDatasetInfo,
	getExistingDatasets,
	parseJSONtoSections,
	unzipContent,
	writeFilesToDisk,
} from "../utils/JsonHelper";
import fs, { readJson } from "fs-extra";
import {
	extractDatasetId,
	getAllSections,
	parseRoomsToInsightResult,
	parseSectionsToInsightResult,
	queryInsightResults,
} from "../utils/QueryHelper";
import { Meta, Section } from "../models/Section";
import { validateQuery } from "../utils/ValidateHelper";
import path from "node:path";
import { parseBuildingStrings, parseIndexString } from "../utils/HTMLHelper";
import { Room } from "../models/Room";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public datasetIds: Map<string, number>;
	public nextAvailableName: number;
	public loadedSections: Map<string, Set<Section>>;
	public loadedRooms: Map<string, Set<Room>>;
	public datasetInfo: Map<string, InsightDataset>;

	constructor() {
		this.datasetIds = new Map<string, number>();
		this.nextAvailableName = 0;
		this.loadedSections = new Map<string, Set<Section>>();
		this.loadedRooms = new Map<string, Set<Room>>();
		this.datasetInfo = new Map<string, InsightDataset>();
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		await this.initializeFields();

		//1) check kind of dataset
		if (kind !== (InsightDatasetKind.Sections || InsightDatasetKind.Rooms)) {
			throw new InsightError("Dataset not of valid kind (Sections or Rooms), could not add dataset");
		}
		//2) Check validity of id: can not be only white space, can not have underscores, reject if id is already in database
		try {
			checkValidId(id, Array.from(this.datasetIds.keys()), false); // adjusted function for map
		} catch (error) {
			throw new InsightError("id passed to addDataset invalid" + error); //is this catch block necessary?
		}

		//3) Unzips content: checks for valid content, must be a base64-encoded string, all valid courses must be contained within courses folder
		const unzipped = await unzipContent(content);
		const fileStringsPromises = extractFileStrings(unzipped, kind);

		//4) parse to Sections in memory and write files to disk
		//Adapted from ChatGPT generated response
		let fileStrings: string[];
		const added = new Set<any>();
		try {
			fileStrings = await Promise.all(fileStringsPromises);

			if (kind === InsightDatasetKind.Sections) {
				for (const fileString of fileStrings) {
					parseJSONtoSections(fileString).forEach((section) => added.add(section));
					await writeFilesToDisk(fileStrings, this.nextAvailableName, id);
				}
			} else if (kind === InsightDatasetKind.Rooms) {
				const indexHTML = unzipped.file("index.htm");
				if (!indexHTML) {
					throw new InsightError("index.htm not found in dataset");
				}
				const indexString = indexHTML.async("string");
				const buildings = parseIndexString(indexString);
				const roomsDataset = parseBuildingStrings(fileStrings, buildings);
				roomsDataset.forEach((room) => added.add(room));

				for (const fileString of fileStrings) {
					await writeFilesToDisk(fileStrings, this.nextAvailableName, id);
				}
			}
		} catch (error) {
			throw new InsightError("unable to convert all sections to JSON formatted strings" + error);
		}

		//5) update datasetIds
		const insightDataset = {
			id: id,
			kind: kind,
			numRows: added.size,
		};
		this.datasetInfo.set(id, insightDataset);
		this.datasetIds.set(id, this.nextAvailableName);
		this.nextAvailableName++;
		if (kind === InsightDatasetKind.Sections) {
			this.loadedSections.set(id, added);
		} else {
			this.loadedRooms.set(id, added);
		}
		//Check to make sure name corresponds to position in datasetIds array
		return Array.from(this.datasetIds.keys());
	}

	public async removeDataset(id: string): Promise<string> {
		await this.initializeFields();

		checkValidId(id, Array.from(this.datasetIds.keys()), true); // 3rd parameter should be true
		try {
			// remove from disk
			const fileName = this.datasetIds.get(id);
			const datasetPath = path.resolve("./data", String(fileName));
			await fs.remove(datasetPath); // txt file?
			// remove from datasetId array
			this.datasetIds.delete(id);
			this.datasetInfo.delete(id);
			this.loadedSections.delete(id);

			//remove from meta
			const metaData: Meta[] = await readJson("./data/meta");
			const newMeta = metaData.filter((meta) => {
				return meta.id !== id;
			});
			await fs.outputFile("./data/meta", JSON.stringify(newMeta));
			// return removed id
			return id;
		} catch (error: any) {
			throw new InsightError(`Error removing dataset with id "${id}": ${error.message}`);
		}
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		// 1) validate query
		const validatedQuery = validateQuery(query);

		// 2) extract dataset id from validated query, ensure dataset exists
		const id = extractDatasetId(validatedQuery);
		if (!this.datasetIds.has(id)) {
			throw new InsightError(`Dataset '${id}' does not exist.`);
		}

		// 3) start with data for all sections
		let allResults: Set<InsightResult>;
		if (this.datasetInfo.get(id)?.kind === InsightDatasetKind.Sections) {
			const allSections = this.loadedSections.get(id);
			if (typeof allSections === "undefined") {
				allResults = await getAllSections(validatedQuery, this.datasetIds);
			} else {
				allResults = parseSectionsToInsightResult(allSections, id);
			}
		} else {
			const allRooms = this.loadedRooms.get(id);
			if (typeof allRooms === "undefined") {
				allResults = await getAllRooms(validatedQuery, this.datasetIds);
			} else {
				allResults = parseRoomsToInsightResult(allRooms, id);
			}
		}

		return queryInsightResults(allResults, validatedQuery);
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		await this.initializeFields();
		const datasetPromises: Promise<InsightDataset>[] = [];

		// get datasets in datasetIds array
		let result: InsightDataset[] = [];
		for (const id of this.datasetIds.keys()) {
			const fileName = String(this.datasetIds.get(id));
			const existingResult = this.datasetInfo.get(id);
			if (typeof existingResult !== "undefined") {
				result = result.concat(existingResult);
			} else {
				datasetPromises.push(getDatasetInfo(String(id), fileName));
			}
		}
		// list id, kind, and numRows
		try {
			return result.concat(await Promise.all(datasetPromises));
		} catch (error: any) {
			throw new InsightError("Failed to list datasets: " + error.message);
		}
	}

	public async initializeFields(): Promise<void> {
		if (this.datasetIds.size === 0) {
			const parts = await getExistingDatasets();
			this.datasetIds = parts[0];
			this.nextAvailableName = parts[1];
		}
	}
}
