import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError, ResultTooLargeError
} from "./IInsightFacade";
import JSZip from "jszip";
import fs from "fs-extra";
import {checkValidId, parseJSONtoSections, writeFilesToDisk} from "../utils/JsonHelper";
import path from "node:path";
import {extractDatasetId, processQueryOnDataset, validateQuery} from "../utils/QueryHelper";


/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public datasetIds: string[];

	//don't include any async code inside the constructor and make the constructor as simple as possible.
	//A function itself could fail/succeed but a constructor should always pass.
	constructor() {
		this.datasetIds = [];
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		//1) check kind of dataset
		if (kind !== InsightDatasetKind.Sections) {
			throw new InsightError("Dataset not of kind InsightDatasetKind.Sections, could not add dataset");
		}

		//2) Check validity of id: can not be only white space, can not have underscores, reject if id is already in database
		try {
			checkValidId(id, this.datasetIds);
		} catch (e) {
			throw new InsightError('id passed to addDataset invalid' + e); //is this catch block necessary?
		}
		//3) Check validity of content: must be a valid base24 string. Must contain at least 1 valid section(not be empty)
		//4) Check validity of courses folder: must be a JSON formatted file, must contain 1 or more valid sections within the result key
		//must be located within a folder called courses/ in root zips directory.
		//A valid section must contain all queryable fields: id, Course, Title, Professor, Subject, Year, Avg, Pass, Fail, Audit

		//unzipping zip file: following JZip gitHub guide: https://stuk.github.io/jszip/documentation/examples.html
		const zip = new JSZip();
		const unzipped = await zip.loadAsync(content, {base64: true})
		//forEach documentation: https://stuk.github.io/jszip/documentation/api_jszip/for_each.html
		const fileStringsPromises: Promise<string>[] = [];
		const test = await unzipped.files['courses/CONS481'].async('string');
		parseJSONtoSections(test);
		unzipped.forEach((relativePath, file) => {
			if (!file.dir) {
				//add promise to array
				fileStringsPromises.push(unzipped.files[relativePath].async('string'))
			}
		})

		//Adapted from ChatGPT generated response
		let fileStrings: string[]
		try {
			fileStrings = await Promise.all(fileStringsPromises);
			for (const fileString of fileStrings) {
				parseJSONtoSections(fileString);
			}
			await writeFilesToDisk(fileStrings, id);
		} catch (error) {
			throw new InsightError("unable to convert all files to JSON formatted strings" + error);
		}
		//append newly added datasetId to list of used ids
		this.datasetIds.push(id);
		return fileStrings;
	}


// 	let courses;
// 	try {
// 	courses = unzipped.folder('courses');
// } catch (error) {
// 	throw new InsightError("Failed to load files, no courses folder found" + error);
// }

	public async removeDataset(id: string): Promise<string> {
		// TODO: Remove this once you implement the methods!

		// validate id: if "", contains _, or only whitespace
		// if (!id || id.includes("_") || id.trim() === "") {
		// 	throw new InsightError("Invalid dataset id.");
		// }
		checkValidId(id, this.datasetIds); // 3rd parameter should be true

		// check if dataset exists
		const datasetIndex = this.datasetIds.indexOf(id);
		if (datasetIndex === -1) {
			throw new NotFoundError(`Dataset with id "${id}" not found.`); // steal this
		}

		try {
			// remove from memory
			// ..
			// ..

			// remove from disk
			await fs.promises.unlink(`data/${id}`); // txt file?

			// remove from datasetId array
			this.datasetIds.splice(datasetIndex, 1);

			// return removed id
			return id;
		} catch (error: any) {
			throw new InsightError(`Error removing dataset with id "${id}": ${error.message}`);
		}
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		// TODO: Remove this once you implement the methods!
		const MAX_SIZE = 5000;

		// 1) validate query
		if (!validateQuery(query)) { // TODO: write helper function in QueryHelper.ts
			throw new InsightError("Invalid query format.");
		}

		// 2) extract dataset id from query
		const id = extractDatasetId(query); // TODO: write helper function in QueryHelper.ts

		// 3) ensure dataset exists
		if (!this.datasetIds.includes(id)) {
			throw new InsightError(`Dataset '${id}' does not exist.`);
		}

		// 4) process query on the dataset
		let results: InsightResult[];
		try {
			results = await processQueryOnDataset(query, id); // TODO: write helper function in QueryHelper.ts
		} catch (error: any) {
			throw new InsightError(`Error processing query: ${error.message}`);
		}

		// 5) handle results that are too large
		if (results.length > MAX_SIZE) {
			throw new ResultTooLargeError("Query results exceed maximum size (5000 sections).");
		}

		return results;
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		// TODO: Remove this once you implement the methods!

		const datasetPromises: Promise<InsightDataset>[] = [];

		// get datasets in datasetIds array
		for (const id of this.datasetIds) {
			datasetPromises.push(this.getDatasetInfo(id)); // need to write this
		}
		// list id, kind, and numRows
		try {
			return await Promise.all(datasetPromises);
		} catch (error: any) {
			throw new InsightError("Failed to list datasets: " + error.message);
		}
	}

	public async getDatasetInfo(id: string): Promise<InsightDataset> {
		checkValidId(id, this.datasetIds); // 3rd parameter false

		// get dataset file path
		const datasetPath = path.resolve(__dirname, '../data', id); // txt file?
		try {
			// read dataset file from disk
			const data = await fs.readFile(datasetPath, 'utf8');
			const dataset = JSON.parse(data);

			// count the number of sections (rows) in the dataset
			const numRows = dataset.sections.length;
			const kind: InsightDatasetKind = InsightDatasetKind.Sections;

			// Return dataset info
			return {
				id: id,
				kind: kind,
				numRows: numRows
			};
		} catch (error: any) {
			throw new InsightError(`Failed to retrieve dataset info for id '${id}': ${error.message}`);
		}
	}
}

// const fse = require('fs-extra')
// fs.readFileSync()
// fs.writeFileSync('wheretowritethefile.json', variablecontainingfile)
// // fs.readFile('filePath', characterEncoding, callbackFunction(error, data))
// if(relativePath.endsWith('.json')) {
// 	const jsonContent = await file.async('text')
// 	const queryJson = JSON.parse(jsonContent); JSON parse only takes a valid json string
// }
//could wrap these in a try catch:
// const jsonData = await fs.readJson(inputFilePath)
// await fs.outputJson(outputFilePath, jsonData)

// save entire dataset json file to disk.
// helper function inside utils folder.
