
import {InsightError} from "../controller/IInsightFacade";
import {JSONFile, Section} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";

/**
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 * @param id
 * @param datasetIds
 */
export function checkValidId(id: string, datasetIds: string[]): void {
    const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
    if (!validId.test(id) || id.trim().length === 0) { //Adapted from chatGPT generated response.
        throw new InsightError(`id provided to addDataset not valid - id=${id};`);
    }
    if (datasetIds.includes(id)) {
        throw new InsightError(`id provided to addDataset already in database - id=${id};`)
    }
}

/**
 * @param section - A section found within file passed to parseJSONtoSections. ASSUME param passed in the form of a
 * JSONFile containing all queryable fields specified in file format
 * @returns - Section, creates a Section interface from JSON object including all queryable fields
 * Will throw an InsightDatasetError if a queryable field can not be found in the JSON object
 */
export function parseSectionObject(section: JSONFile): Section {  //I would like to specify that it is a Section object if possible

    const fields: string[] = ["Year", "Subject", "Title", "id", "Professor", "Avg", "Pass", "Fail", "Course"]
    for (const field of fields) {
        if (!Object.prototype.hasOwnProperty.call(section, field)) {
            throw new InsightError("section passed to parseSectionObject does not contain all queryable fields")
        }
    }

    let newSection: Section;
    try {
        newSection = {  //I like it this way it is more explicit.
            uuid: String(section.id),  //how to I specify that the parameter contains these fields.
            id: section.Course,
            title: section.Title,
            instructor: section.Professor,
            dept: section.Subject,
            year: Number(section.Year),
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
 * @param file - A file found within /courses directory. ASSUME contents is a string in JSON format
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export function parseJSONtoSections(file: string): Section[] {
    const addedSections: Section[] = [];

    try {
        const sections = JSON.parse(file).result as JSONFile[];

        for (const section of sections) {
            const newSection = parseSectionObject(section);
            addedSections.push(newSection);
            console.log(newSection);
        }
    } catch(error) {
        throw new InsightError("Unable to parse to JSON, file is not a JSON formatted string" + error);
    }

    return addedSections;
}

/**
 * @param files - Files contained within an added Dataset, ASSUME files is a list of JSON formatted strings
 * @param id
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export async function writeFilesToDisk(files: string[], id: string): Promise<void> {
    let acc: object = {}; //this might cause problems down the line
    for (const file of files) {
        const JSONObject = JSON.parse(file)
        //Adapted from ChatGPT generated response
        acc = {...acc, ...JSONObject}
    }
    //Adapted from ChatGPT generated response
    const idPath = path.resolve(__dirname, '../data', id);
    console.log(idPath);
    try{
        //await fs.outputFile(idPath, 'test, please work!');
        await fs.outputFile(idPath, JSON.stringify(acc)); //How can I add the space argument?
        console.log('made output file');
    }catch (error) {
        throw new InsightError("failed to write files to disk" + id + error);
    }
}

//Why in the world can't I use then catch?!
// fs.outputJSON(idPath, files).then(() => {
//     return acc;
// }).catch(() => {
//     throw new InsightError("Failed to write file to disk");
// })

