
import {InsightError} from "../controller/IInsightFacade";
import {Section} from "../models/Section";

/**
 * @param section - A section found within file passed to parseJSONtoSections
 * @returns - Section, creates a Section interface from JSON object including all queryable fields
 * Will throw an InsightDatasetError if a queryable field can not be found in the JSON object
 */
export function parseSectionObject(section: object): object {  //I would like to specify that it is a Section object if possible

    let newSection: Section;
    try {
        newSection = {  //I like it this way it is more explicit.
            uuid: section.id,  //how to I specify that the parameter contains these fields.
            id: section.Course,
            title: section.Title,
            instructor: section.Professor,
            dept: section.Subject,
            year: section.Year,
            avg: section.Avg,
            pass: section.Pass,
            fail: section.Fail,
        }
    } catch (error) {
        throw new InsightError("failed to create new Section Object in parse Section Object" + error);
    }
        return newSection;
}

/**
 * @param file - A file found within /courses directory in JSON format
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export function parseJSONtoSections(file: string): Section[] {
    const addedSections: Section[] = [];

    try {
        const sections = JSON.parse(file).result;

        for (const section of sections) {
            parseSectionObject(section);
            addedSections.push(section);
        }
    } catch(error) {
        throw new InsightError("Unable to parse to JSON, file is not a JSON formatted string" + error);
    }

    return addedSections;
}

/**
 * @param files - Files contained within an added Dataset
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export function writeFilesToDisk(files: string[], id: string): void {
    console.log(files);

}

/**
 * @param file - A file found within /courses directory in JSON format
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
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