import { IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, InsightResult } from "./IInsightFacade";
import {
	addSectionsDataset,
	checkValidId,
	extractFileStrings,
	getDatasetInfo,
	getExistingDatasets,
	unzipContent,
} from "../utils/JsonHelper";
import fs, { readJson } from "fs-extra";
import {
	extractDatasetId,
	getAllData,
	loadMeta,
	parseRoomsToInsightResult,
	parseSectionsToInsightResult,
	queryInsightResults,
} from "../utils/QueryHelper";
import { Meta, Section } from "../models/Section";
import { validateQuery } from "../utils/ValidateHelper";
import path from "node:path";
import { Room } from "../models/Room";
import { addRoomsDataset } from "../utils/HTMLHelper";

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

		// 1) check kind of dataset
		if (kind !== InsightDatasetKind.Sections && kind !== InsightDatasetKind.Rooms) {
			throw new InsightError("Dataset not of valid kind (Sections or Rooms), could not add dataset");
		}
		// 2) Check validity of id: can not be only white space, can not have underscores, reject if id is already in database
		checkValidId(id, Array.from(this.datasetIds.keys()), false); // adjusted function for map

		// 3) Unzips content: checks for valid content
		const unzipped = await unzipContent(content);
		const fileStrings = await extractFileStrings(unzipped, kind);

		// 4) Write files to disk
		let addedData;
		let numRows = 0;
		try {
			if (kind === InsightDatasetKind.Sections) {
				addedData = await addSectionsDataset(fileStrings, this.nextAvailableName, id);
				this.loadedSections.set(id, addedData);
				numRows = addedData.size;
			} else if (kind === InsightDatasetKind.Rooms) {
				addedData = await addRoomsDataset(unzipped, fileStrings, this.nextAvailableName, id);
				this.loadedRooms.set(id, addedData);
				numRows = addedData.size;
			}
		} catch (error) {
			throw new InsightError("Unable to convert all sections to JSON formatted strings" + error);
		}

		//5) update datasetIds
		const insightDataset = {
			id: id,
			kind: kind,
			numRows: numRows,
		};
		this.datasetInfo.set(id, insightDataset);
		this.datasetIds.set(id, this.nextAvailableName);
		this.nextAvailableName++;

		return Array.from(this.datasetIds.keys());
	}

	public async removeDataset(id: string): Promise<string> {
		await this.initializeFields();

		checkValidId(id, Array.from(this.datasetIds.keys()), true); // 3rd parameter should be true
		try {
			// remove from disk
			const fileName = this.datasetIds.get(id);
			const datasetPath = path.resolve("./data", String(fileName));
			await fs.remove(datasetPath);
			// remove from datasetId array\
			const datasetInfo = this.datasetInfo.get(id);
			if (datasetInfo) {
				if (datasetInfo.kind === InsightDatasetKind.Sections) {
					this.loadedSections.delete(id);
				} else {
					this.loadedRooms.delete(id);
				}
				this.datasetInfo.delete(id);
			}
			this.datasetIds.delete(id);

			// remove from metadata file
			const metaData: Meta[] = await readJson("./data/meta");
			const newMeta = metaData.filter((meta) => meta.id !== id);
			await fs.outputFile("./data/meta", JSON.stringify(newMeta));
			// return removed id
			return id;
		} catch (error: any) {
			throw new InsightError(`Error removing dataset with id "${id}": ${error.message}`);
		}
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		// 2) extract dataset id from validated query, ensure dataset exists
		const id = extractDatasetId(query, this.datasetIds);

		// 3) start with data for all sections
		let allResults: Set<InsightResult>;
		let kind = this.datasetInfo.get(id)?.kind;
		if (!kind) {
			this.datasetInfo = await loadMeta();
			kind = this.datasetInfo.get(id)?.kind;
		}
		let validatedQuery;
		if (kind === InsightDatasetKind.Sections) {
			validatedQuery = validateQuery(query, kind);
			const allSections = this.loadedSections.get(id);
			if (typeof allSections === "undefined") {
				allResults = await getAllData(validatedQuery, this.datasetIds, kind);
			} else {
				allResults = parseSectionsToInsightResult(allSections, id);
			}
		} else if (kind === InsightDatasetKind.Rooms) {
			validatedQuery = validateQuery(query, kind);
			const allRooms = this.loadedRooms.get(id);
			if (typeof allRooms === "undefined") {
				allResults = await getAllData(validatedQuery, this.datasetIds, kind);
			} else {
				allResults = parseRoomsToInsightResult(allRooms, id);
			}
		} else {
			throw new InsightError("invalid kind stored in datasetIds");
		}

		return queryInsightResults(allResults, validatedQuery);
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		await this.initializeFields();
		const datasetPromises: Promise<InsightDataset>[] = [];

		// get datasets in datasetIds array
		let result: InsightDataset[] = [];
		for (const id of this.datasetIds.keys()) {
			const existingResult = this.datasetInfo.get(id);
			// if dataset exists
			if (typeof existingResult !== "undefined") {
				result = result.concat(existingResult);
			} else {
				// load datasets from disk
				datasetPromises.push(getDatasetInfo(String(id)));
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
		// what if 1 added after making new instance?
		if (this.datasetIds.size === 0) {
			const parts = await getExistingDatasets();
			this.datasetIds = parts[0];
			this.nextAvailableName = parts[1];
		}
	}
}
