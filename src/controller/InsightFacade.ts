import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, InsightResult} from "./IInsightFacade";
import {
	checkValidId,
	extractFileStrings,
	parseJSONtoSections,
	unzipContent,
	writeFilesToDisk
} from "../utils/JsonHelper";


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

		//3) Unzips content: checks for valid content, must be a base64 encoded string, all valid courses must be contained within courses folder
		const unzipped = await unzipContent(content);
		const fileStringsPromises = extractFileStrings(unzipped);

		//4) parse to Sections in memory and write files to disk
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

		//5) update datasetIds
		this.datasetIds.push(id);
		return fileStrings;
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