import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	ResultTooLargeError,
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
import fs from "fs-extra";
import { extractDatasetId, getAllSections, handleFilter, selectColumns, sortResults } from "../utils/QueryHelper";
import { Query } from "../models/Section";
import { validateQuery } from "../utils/ValidateHelper";
import path from "node:path";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public datasetIds: Map<string, number>;
	public nextAvailableName: number;

	//don't include any async code inside the constructor and make the constructor as simple as possible.
	//A function itself could fail/succeed but a constructor should always pass.
	constructor() {
		this.datasetIds = new Map<string, number>();
		this.nextAvailableName = 0;
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		if (this.datasetIds.size === 0) {
			const parts = await getExistingDatasets();
			this.datasetIds = parts[0];
			this.nextAvailableName = parts[1];
		}

		//1) check kind of dataset
		if (kind !== InsightDatasetKind.Sections) {
			throw new InsightError("Dataset not of kind InsightDatasetKind.Sections, could not add dataset");
		}
		//2) Check validity of id: can not be only white space, can not have underscores, reject if id is already in database
		try {
			checkValidId(id, Array.from(this.datasetIds.keys()), false); // adjusted function for map
		} catch (error) {
			throw new InsightError("id passed to addDataset invalid" + error); //is this catch block necessary?
		}
		//3) Unzips content: checks for valid content, must be a base64 encoded string, all valid courses must be contained within courses folder
		const unzipped = await unzipContent(content);
		const fileStringsPromises = extractFileStrings(unzipped);

		//4) parse to Sections in memory and write files to disk
		//Adapted from ChatGPT generated response
		let fileStrings: string[];
		try {
			fileStrings = await Promise.all(fileStringsPromises);
			for (const fileString of fileStrings) {
				parseJSONtoSections(fileString);
			}
			await writeFilesToDisk(fileStrings, this.nextAvailableName, id);
		} catch (error) {
			throw new InsightError("unable to convert all files to JSON formatted strings" + error);
		}

		//5) update datasetIds
		this.datasetIds.set(id, this.nextAvailableName);
		this.nextAvailableName++;
		//Check to make sure name corresponds to position in datasetIds array
		return Array.from(this.datasetIds.keys());
	}

	public async removeDataset(id: string): Promise<string> {
		if (this.datasetIds.size === 0) {
			const parts = await getExistingDatasets();
			this.datasetIds = parts[0];
			this.nextAvailableName = parts[1];
		}

		checkValidId(id, Array.from(this.datasetIds.keys()), true); // 3rd parameter should be true
		try {
			// remove from disk
			const fileName = this.datasetIds.get(id);
			const datasetPath = path.resolve("./data", String(fileName));
			await fs.promises.unlink(datasetPath); // txt file?
			// remove from datasetId array
			this.datasetIds.delete(id);
			// return removed id
			return id;
		} catch (error: any) {
			throw new InsightError(`Error removing dataset with id "${id}": ${error.message}`);
		}
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		if (this.datasetIds.size === 0) {
			const parts = await getExistingDatasets();
			this.datasetIds = parts[0];
			this.nextAvailableName = parts[1];
		}
		const MAX_SIZE = 5000;

		// 1) validate query
		let validatedQuery: Query;
		try {
			validatedQuery = validateQuery(query);
		} catch (error) {
			throw new InsightError(`Query not a valid format: ` + error);
		}

		// 2) extract dataset id from validated query, ensure dataset exists
		const id = extractDatasetId(validatedQuery);
		if (!this.datasetIds.has(id)) {
			throw new InsightError(`Dataset '${id}' does not exist.`);
		}

		// process query on the dataset

		// 3) start with data for all sections
		const allSections = await getAllSections(validatedQuery, this.datasetIds);

		// 4) filter results if necessary (WHERE)
		let filteredResults: InsightResult[];
		filteredResults = handleFilter(validatedQuery.WHERE, allSections);

		// 5) handle results that are too large
		if (filteredResults.length > MAX_SIZE) {
			throw new ResultTooLargeError("Query results exceed maximum size (5000 sections).");
		}

		// 6) select only specified columns (OPTIONS.COLUMNS)
		filteredResults = selectColumns(filteredResults, validatedQuery);

		// 7) sort results if necessary (OPTIONS.ORDER)
		let sortedFilteredResults: InsightResult[];
		if (validatedQuery.OPTIONS.ORDER) {
			sortedFilteredResults = sortResults(validatedQuery.OPTIONS, filteredResults);
		} else {
			sortedFilteredResults = filteredResults;
		}
		return sortedFilteredResults;
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		if (this.datasetIds.size === 0) {
			const parts = await getExistingDatasets();
			this.datasetIds = parts[0];
			this.nextAvailableName = parts[1];
		}

		const datasetPromises: Promise<InsightDataset>[] = [];

		// get datasets in datasetIds array
		for (const id of this.datasetIds.keys()) {
			const fileName = String(this.datasetIds.get(id));
			datasetPromises.push(getDatasetInfo(String(id), fileName)); // need to write this
		}
		// list id, kind, and numRows
		try {
			return await Promise.all(datasetPromises);
		} catch (error: any) {
			throw new InsightError("Failed to list datasets: " + error.message);
		}
	}
}
