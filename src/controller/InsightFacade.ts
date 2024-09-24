import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, InsightResult} from "./IInsightFacade";
import {DatabaseInfo} from "./Dataset";
import JSZip from "jszip";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		// TODO: Remove this once you implement the methods!
		//1) check kind of dataset
		if (kind !== InsightDatasetKind.Sections) {
			throw new InsightError("Dataset not of kind InsightDatasetKind.Sections, could not add dataset");
		}

		//2) Check validity of id: can not be only white space, can not have underscores, reject if id is already in database
		const validId =  /^[^_]+$/; //Adapted from chatGPT generated response.
		if(!validId.test(id)) { //Adapted from chatGPT generated response.
			throw new InsightError(`id provided to addDataset not valid - id=${id};`);
		}
		if(DatabaseInfo.getInstance().datasetIds.includes(id)) {
			throw new InsightError(`id provided to addDataset already in database - id=${id};`)
		}

		//3) Check validity of content: must be a valid base24 string. Must contain at least 1 valid section(not be empty)
		//4) Check validity of courses folder: must be a JSON formatted file, must contain 1 or more valid sections within the result key
		//must be located within a folder called courses/ in root zips directory.
		//A valid section must contain all queryable fields: id, Course, Title, Professor, Subject, Year, Avg, Pass, Fail, Audit

		//Decoding a base64 string: adapted from StackOverflow answer
		const decode = (str: string):string => Buffer.from(str, 'base64').toString('binary');
		//unzipping zip file: following JZip github guide: https://stuk.github.io/jszip/documentation/examples.html
		//They suggest: are these necessary?
		//var fs = require("fs")
		//var JSZip = require("jszip")
		const zip = new JSZip();
		zip.loadAsync(decode(content)).then(data => {
			//forEach documentation: https://stuk.github.io/jszip/documentation/api_jszip/for_each.html
			//should execute callback function for each entry at this folder level.
			zip.forEach((relativePath, file) => {
				if(file.dir && relativePath === 'courses/') {
					//good
				} else {
					throw new InsightError('add dataset received invalid content, zip file does not contain courses/')
				}

			})

		}).catch(error) {

		}


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
