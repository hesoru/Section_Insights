import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, InsightResult} from "./IInsightFacade";
import JSZip from "jszip";
import fs from "fs-extra";
import {checkValidId, parseJSONtoSections, writeFilesToDisk} from "../utils/JsonHelper";


/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public datasetIds: string[];

	//dont include any async code inside the constructor and make the constructor as simple as possible.
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

		//Decoding a base64 string: adapted from StackOverflow answer
		//const decode = (str: string): string => Buffer.from(str, 'base64').toString('binary');
		//unzipping zip file: following JZip github guide: https://stuk.github.io/jszip/documentation/examples.html

		const zip = new JSZip();
		const unzipped = await zip.loadAsync(content, {base64: true})
		// 	//forEach documentation: https://stuk.github.io/jszip/documentation/api_jszip/for_each.html
		// 	//should execute callback function for each entry at this folder level.
		//const coursesFolder = unzipped.folder("courses");
		const fileStringsPromises: Promise<string>[] = [];
		const test = await unzipped.files['courses/ANTH312'].async('string');
		parseJSONtoSections(test);
		// @ts-ignore
		unzipped.forEach((relativePath) => {
				fileStringsPromises.push(unzipped.files[relativePath].async('string')) //add promise to array
			})

		//Adapted from ChatGPT generated response
		try {
			const fileStrings = await Promise.all(fileStringsPromises);
			for (const fileString of fileStrings) {
				parseJSONtoSections(fileString);
			}
			writeFilesToDisk(fileStrings);
		} catch (error) {
			throw new InsightError("unable to convert all files to JSON formatted strings" + error);
		}
		//try catch for promise.all
		//array of sections equals return of helper and store array of section to disk.
		//loop through promises array
		return [];
	}

	public async removeDataset(id: string): Promise<string> {
		// TODO: Remove this once you implement the methods!
		throw new Error(`InsightFacadeImpl::removeDataset() is unimplemented! - id=${id};`);
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		// TODO: Remove this once you implement the methods!
		throw new Error(`InsightFacadeImpl::performQuery() is unimplemented! - query=${query};`);
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		// TODO: Remove this once you implement the methods!
		throw new Error(`InsightFacadeImpl::listDatasets is unimplemented!`);
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