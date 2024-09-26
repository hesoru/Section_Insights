/**
 *
 * @param file - A file found within /courses directory in JSON format
 * @returns - void, separates file into its individual sections and creates a Sections object from each
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
import {InsightError} from "../controller/IInsightFacade";

export function parseJSONtoSections(file: string): void {

    console.log(file);

}

export function writeFilesToDisk(files: string[]): void {
    console.log(files);

}

export function checkValidId(id: string, datasetIds: string[]): void {
    const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
    if (!validId.test(id)) { //Adapted from chatGPT generated response.
        //throw new InsightError(`id provided to addDataset not valid - id=${id};`);
    }
    if (datasetIds.includes(id)) {
        throw new InsightError(`id provided to addDataset already in database - id=${id};`)
    }
}

// // 	//Adapted from chatGPT generated response
// const coursesFolder = zip.folder("/courses");
// if (!coursesFolder) {
//     throw new InsightError('add dataset received invalid content, no /courses folder found');
// }
//const result =	await unzipped.files[relativePath].async('string')