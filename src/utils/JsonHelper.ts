import { InsightDataset, InsightDatasetKind, InsightError } from "../controller/IInsightFacade";
import { JSONFile, Section } from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import JSZip from "jszip";

/**
 * @returns - true if id is a valid dataset id and has not already been used in the database
 * Will throw an InsightError otherwise
 * @param id
 * @param datasetIds
 * @param includes
 */
export function checkValidId(id: string, datasetIds: string[], includes: boolean): boolean {
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(id) || id.trim().length === 0) {
		//Adapted from chatGPT generated response.
		throw new InsightError(`id provided to addDataset not valid - id=${id};`);
	}
	if (includes) {
		if (!datasetIds.includes(id)) {
			throw new InsightError(`id provided to removeDataset is not in database - id=${id};`);
		}
	} else {
		if (datasetIds.includes(id)) {
			throw new InsightError(`id provided to addDataset already in database - id=${id};`);
		}
	}
	return true;
}

//checkValidId(id, this.datasetIds, false) <- will say it's a valid id if the id provided is already in the list of datasetIds

/**
 * @param section - A section found within file passed to parseJSONtoSections. ASSUME param passed in the form of a
 * JSONFile containing all queryable fields specified in file format
 * @returns - Section, creates a Section interface from JSON object including all queryable fields
 * Will throw an InsightDatasetError if a queryable field can not be found in the JSON object
 */
export function parseSectionObject(section: JSONFile): Section {
	//I would like to specify that it is a Section object if possible

	const fields: string[] = ["Year", "Subject", "Title", "id", "Professor", "Avg", "Pass", "Fail", "Course"];
	for (const field of fields) {
		if (!Object.prototype.hasOwnProperty.call(section, field)) {
			throw new InsightError("section passed to parseSectionObject does not contain all queryable fields");
		}
	}

	let newSection: Section;
	try {
		newSection = {
			//I like it this way it is more explicit.
			uuid: String(section.id), //how to I specify that the parameter contains these fields.
			id: section.Course,
			title: section.Title,
			instructor: section.Professor,
			dept: section.Subject,
			year: Number(section.Year),
			avg: section.Avg,
			pass: section.Pass,
			fail: section.Fail,
			audit: section.Audit,
		};
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
		}
	} catch (error) {
		throw new InsightError("Unable to parse to JSON, file is not a JSON formatted string" + error);
	}

	return addedSections;
}

/**
 * @param files - Files contained within an added Dataset, ASSUME files is a list of JSON formatted strings
 * @param name
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export async function writeFilesToDisk(files: string[], name: number): Promise<number> {
	const acc = []; //this might cause problems down the line
	for (const file of files) {
		const JSONObject = JSON.parse(file);
		//Adapted from ChatGPT generated response
		acc.push(JSONObject);
	}
	//Adapted from ChatGPT generated response
	const idPath = path.resolve(__dirname, "../data", String(name));
	try {
		await fs.outputFile(idPath, JSON.stringify(acc, null, 2)); //How can I add the space argument?
	} catch (error) {
		throw new InsightError("failed to write files to disk" + name + error);
	}

	return name;
}

/**
 * @param unzipped - JSZip object of the contents to be added to the database.
 * First checks to see if courses directory exists, any valid courses must be contained inside the courses' directory.
 * If no courses directory found throws InsightError
 * Second extracts the contents of each file as a string. If another directory is contained inside courses throws an Insight Error
 * Third checks that there is at least one file extracted from courses, otherwise throws InsightError
 * @returns - Promise<string>[], when all promises are resolved will be an array of file contents.
 */
export function extractFileStrings(unzipped: JSZip): Promise<string>[] {
	//forEach documentation: https://stuk.github.io/jszip/documentation/api_jszip/for_each.html
	const fileStringsPromises: Promise<string>[] = [];
	const courses = unzipped.folder("courses/");
	if (!courses) {
		throw new InsightError("courses folder not found in file");
	}
	courses.forEach((relativePath, file) => {
		if (file.dir) {
			throw new InsightError("file " + relativePath + "is a folder within courses folder");
		}
		fileStringsPromises.push(file.async("string"));
	});

	if (fileStringsPromises.length === 0) {
		throw new InsightError("file does not contain at least one valid section");
	}

	return fileStringsPromises;
}

/**
 * @param content - base64 formatted string encoding zip file containing dataset.
 * Unzips content into JSZip object to be further processed by extractFileStrings
 * If content can not be unzipped throws InsightError
 * @returns - JSZip object of unzipped dataset
 */
export async function unzipContent(content: string): Promise<JSZip> {
	//unzipping zip file: following JZip gitHub guide: https://stuk.github.io/jszip/documentation/examples.html
	const zip = new JSZip();
	let unzipped: JSZip;
	try {
		unzipped = await zip.loadAsync(content, { base64: true });
	} catch (error) {
		throw new InsightError("content passed to addDataset is not a valid base64 string" + error);
	}
	return unzipped;
}

//Why in the world can't I use then catch?!
// fs.outputJSON(idPath, files).then(() => {
//     return acc;
// }).catch(() => {
//     throw new InsightError("Failed to write file to disk");
// })

export async function getDatasetInfo(id: string): Promise<InsightDataset> {
	//checkValidId(id, this.datasetIds, true); // 3rd parameter true

	// get dataset file path
	const datasetPath = path.resolve(__dirname, "../data", id); // txt file?
	try {
		// read dataset file from disk
		const data = await fs.readFile(datasetPath, "utf8");
		const dataset = JSON.parse(data);

		// count the number of sections (rows) in the dataset
		const numRows = dataset.sections.length;
		const kind: InsightDatasetKind = InsightDatasetKind.Sections;

		// Return dataset info
		return {
			id: id,
			kind: kind,
			numRows: numRows,
		};
	} catch (error: any) {
		throw new InsightError(`Failed to retrieve dataset info for id '${id}': ${error.message}`);
	}
}
