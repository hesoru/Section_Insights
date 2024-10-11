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
	parseJSONtoSections,
	unzipContent,
	writeFilesToDisk,
	getDatasetInfo,
} from "../utils/JsonHelper";
import fs from "fs-extra";
import { extractDatasetId, getAllSections, handleFilter, sortResults } from "../utils/QueryHelper";
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
	public nextAvailableName = 0;

	//don't include any async code inside the constructor and make the constructor as simple as possible.
	//A function itself could fail/succeed but a constructor should always pass.
	constructor() {
		this.datasetIds = new Map<string, number>();
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		// 1) Check kind of dataset
		if (kind !== InsightDatasetKind.Sections) {
			throw new InsightError("Dataset not of kind InsightDatasetKind.Sections, could not add dataset");
		}
		// 2) Check validity of id: can not be only white space, can not have underscores, reject if id is already in database
		try {
			checkValidId(id, Array.from(this.datasetIds.keys()), false); // adjusted function for map
		} catch (error) {
			throw new InsightError("id passed to addDataset invalid" + error); //is this catch block necessary?
		}

		// 3) Unzips content: checks for valid content, must be a base64 encoded string, all valid courses must be contained within courses folder
		const unzipped = await unzipContent(content);
		const fileStringsPromises = extractFileStrings(unzipped);

		// 4) Parse to Sections in memory and write files to disk
		// Inspired by ChatGPT responses
		let fileStrings: string[];
		try {
			fileStrings = await Promise.all(fileStringsPromises);
			for (const fileString of fileStrings) {
				parseJSONtoSections(fileString);
			}
			await writeFilesToDisk(fileStrings, this.nextAvailableName);
		} catch (error) {
			throw new InsightError("unable to convert all files to JSON formatted strings" + error);
		}

		// 5) Update datasetIds
		this.datasetIds.set(id, this.nextAvailableName);
		this.nextAvailableName++;
		// Check to make sure name corresponds to position in datasetIds array

		return Array.from(this.datasetIds.keys());
	}

	public async removeDataset(id: string): Promise<string> {
		// 1) Validate id format and that id exists in InsightFacade (may throw NotFoundError)
		checkValidId(id, Array.from(this.datasetIds.keys()), true); // 3rd parameter should be true

		try {
			// 2) Remove dataset from disk
			const fileName = this.datasetIds.get(id);
			const datasetPath = path.resolve(__dirname, "../data", String(fileName));
			await fs.promises.unlink(datasetPath);
			// 3) If file deletion succeeds, remove id from datasetIds map
			this.datasetIds.delete(id);
			// 4) Return removed id
			return id;
		} catch (error: any) {
			throw new InsightError(`Error removing dataset with id = "${id}": ` + error);
		}
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		// 1) Validate query format
		let validatedQuery: Query;
		try {
			validatedQuery = validateQuery(query);
		} catch (error) {
			throw new InsightError(`Query not a valid format: ` + error);
		}

		// 2) Extract dataset id from validated query and ensure dataset exists
		const id = extractDatasetId(validatedQuery);
		if (!this.datasetIds.has(id)) {
			throw new InsightError(`Dataset "${id}'" does not exist.`);
		}

		// ----- process query on the dataset -----

		// 3) Start with data for all sections as an InsightResult[]
		const allSections = await getAllSections(validatedQuery, this.datasetIds);

		// 4) Filter results if necessary (WHERE)
		let filteredResults: InsightResult[];
		filteredResults = handleFilter(validatedQuery.WHERE, allSections);

		// 5) Handle results that are too large
		const MAX_SIZE = 5000;
		if (filteredResults.length > MAX_SIZE) {
			throw new ResultTooLargeError("Query results exceed maximum size (5000 sections).");
		}

		// 6) Select only specified columns (OPTIONS.COLUMNS)
		// Inspired by ChatGPT responses
		filteredResults = filteredResults.map((section) => {
			// can be string | number
			const result: any = {};
			const columns = validatedQuery.OPTIONS.COLUMNS;
			for (const column of columns) {
				result[column] = section[column];
			}
			return result;
		});

		// 7) Sort results if necessary (OPTIONS.ORDER)
		let sortedFilteredResults: InsightResult[];
		if (validatedQuery.OPTIONS.ORDER) {
			sortedFilteredResults = sortResults(validatedQuery.OPTIONS, filteredResults);
		} else {
			sortedFilteredResults = filteredResults;
		}

		return sortedFilteredResults;
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		const datasetPromises: Promise<InsightDataset>[] = [];

		// Get datasets in datasetIds array
		for (const id of this.datasetIds.keys()) {
			// 1) Obtain file names of all ids in datasetIds
			const fileName = String(this.datasetIds.get(id));
			// 2) Push promises to array: each promise returns an InsightDataset
			datasetPromises.push(getDatasetInfo(String(id), fileName)); // need to write this
		}

		try {
			// 3) Resolve array of promises, returning an InsightDataset[]
			return await Promise.all(datasetPromises);
		} catch (error: any) {
			throw new InsightError("Failed to list datasets: " + error);
		}
	}
}
