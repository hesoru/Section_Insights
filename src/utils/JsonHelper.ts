import {InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "../controller/IInsightFacade";
import {JSONFile, MetadataEntry, Room, Section} from "../models/Section";
import fs, {readJson} from "fs-extra";
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
		throw new InsightError(`id provided to addDataset not valid - id=${id};`);
	}
	if (includes) {
		if (!datasetIds.includes(id)) {
			throw new NotFoundError(`Dataset with id "${id}" not found.`);
		}
	} else {
		if (datasetIds.includes(id)) {
			throw new InsightError(`id provided to addDataset already in database - id=${id};`);
		}
	}
	return true;
}

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
	let newYear = section.Year;
	const overall = 1900;
	if (Object.prototype.hasOwnProperty.call(section, "Section")) {
		if (section.Section === "overall") {
			newYear = overall;
		}
	}
	{
		try {
			newSection = {
				//I like it this way it is more explicit.
				uuid: String(section.id), //how to I specify that the parameter contains these fields.
				id: section.Course,
				title: section.Title,
				instructor: section.Professor,
				dept: section.Subject,
				year: Number(newYear),
				avg: section.Avg,
				pass: section.Pass,
				fail: section.Fail,
				audit: section.Audit,
			};
		} catch (error) {
			throw new InsightError("failed to create new Section Object in parse Section Object" + error);
		}
	}
	return newSection;
}

/**
 * @param file - A file found within /courses directory. ASSUME contents is a string in JSON format
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export function parseJSONtoSections(file: string): Set<Section> {
	const addedSections: Set<Section> = new Set<Section>();
	try {
		const sections = JSON.parse(file).result as JSONFile[];

		for (const section of sections) {
			const newSection = parseSectionObject(section);
			addedSections.add(newSection);
		}
	} catch (error) {
		throw new InsightError("Unable to parse to JSON, file is not a JSON formatted string" + error);
	}

	return addedSections;
}

/**
 * @param fileStrings - Files contained within an added Dataset, ASSUME files is a list of JSON formatted strings
 * @param roomsDataset
 * @param name
 * @param id
 * @param kind
 * @returns - Sections[], separates file into its individual sections passing each section to parseSectionObject and adding the returned object to the array
 * Will throw an InsightDatasetError if file is not a JSON formatted string
 */
export async function writeFilesToDisk(fileStrings: string[] | null, roomsDataset: Room[] | null,
									   name: number, id: string, kind: InsightDatasetKind): Promise<number> {
	let outputObject;
	let datasetSize;
	if (kind === InsightDatasetKind.Sections) {
		if (!fileStrings) {
			throw new InsightError("Section dataset file strings missing!");
		}
		const acc: Section[] = []; //this might cause problems down the line
		for (const file of fileStrings) {
			const JSONObject = JSON.parse(file);
			acc.push(JSONObject);
		}
		outputObject = {datasetID: id, files: acc};
		datasetSize = outputObject.files.length;
	} else {
		if (!roomsDataset) {
			throw new InsightError("Rooms dataset missing!");
		}
		outputObject = {datasetID: id, files: roomsDataset};
		datasetSize = outputObject.files.length;
	}
	const idPath = path.resolve("./data", String(name));
	try {
		const space = 2;
		await fs.outputFile(idPath, JSON.stringify(outputObject, null, space)); //How can I add the space argument?
	} catch (error) {
		throw new InsightError("failed to write files to disk" + name + error);
	}

	return await writeMetadataFile(name, id, kind, datasetSize);
}

export async function writeMetadataFile(name: number, id: string, kind: InsightDatasetKind, datasetSize: number):
	Promise<number> {
	// handle meta:
	const fileMeta = {
		id: id,
		fileName: name,
		kind: kind,
		numRows: datasetSize
	};
	const metaPath = path.resolve("./data", "meta");
	let dataMeta;
	try {
		dataMeta = await readJson(metaPath);
	} catch {
		// initialize new dataMeta array
		dataMeta = [];
	}
	dataMeta.push(fileMeta);
	await fs.outputFile(metaPath, JSON.stringify(dataMeta));
	return name;
}

/**
 * @param unzipped - JSZip object of the contents to be added to the database.
 * First checks to see if courses directory exists, any valid courses must be contained inside the courses' directory.
 * If no courses directory found throws InsightError
 * Second extracts the contents of each file as a string. If another directory is contained inside courses throws an Insight Error
 * Third checks that there is at least one file extracted from courses, otherwise throws InsightError
 * @param kind
 * @returns - Promise<string>[], when all promises are resolved will be an array of file contents.
 */
export function extractFileStrings(unzipped: JSZip, kind: InsightDatasetKind): Promise<string>[] {
	//forEach documentation: https://stuk.github.io/jszip/documentation/api_jszip/for_each.html
	const fileStringsPromises: Promise<string>[] = [];
	let directory: JSZip | null;
	if (kind === InsightDatasetKind.Sections) {
		directory = unzipped.folder("courses/");
		if (!directory) {
			throw new InsightError("courses directory not found in dataset");
		}
	} else {
		directory = unzipped.folder("campus/discover/buildings-and-classrooms/");
		if (!directory) {
			throw new InsightError("campus/discover/buildings-and-classrooms/ directory not found in dataset");
		}
		// filter buildings-and-classrooms/ for HTML files
		directory.filter(file => path.extname(file) === '.htm');
		if (!directory) {
			throw new InsightError("campus/discover/buildings-and-classrooms/ contains no HTML files");
		}
	}

	directory.forEach((relativePath, file) => {
		if (file.dir) {
			throw new InsightError("file " + relativePath + " is a folder within courses folder");
		}
		fileStringsPromises.push(file.async("string"));
	});

	if (fileStringsPromises.length === 0) {
		throw new InsightError("file does not contain at least one valid section or building");
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

/**
 * @returns - Promise<InsightDataset>, gets the dataset info for each dataset in the database, if loadDatasets fails throws InsightError
 * @param id
 * @param fileName
 * @param kind
 */
export async function getDatasetInfo(id: string, fileName: string): Promise<InsightDataset> {
	// shouldn't have to validate id in listDataset()
	// checkValidId(id, datasetIds, true); // 3rd parameter true

	// get dataset file path
	try {
		const metadataPath = path.resolve("./data", fileName);
		const metadataFile = await fs.readJson(metadataPath);
		const metadata = JSON.parse(metadataFile) as MetadataEntry[];
		const datasetInfo = metadata.find((entry) => entry.id === id);

		// Return dataset info
		if (datasetInfo) {
			return {
				id: id,
				kind: datasetInfo?.kind,
				numRows: datasetInfo?.numRows,
			};
		} else {
			throw new InsightError("Dataset info not found in metadata file.");
		}
	// 	const sections = await loadDataset(id, fileName, kind);
	// 	const numRows = sections.size;
	//
	// 	// Return dataset info
	// 	return {
	// 		id: id,
	// 		kind: kind,
	// 		numRows: numRows,
	// 	};
	} catch (error: any) {
		throw new InsightError(`Failed to retrieve dataset info for id '${id}': ${error.message}`);
	}
}

/**
 * @returns - Promise<[Map,string,number>,number]>, initializes a map with the names and ids of each file already existing in the ./data folder
 * Will throw an InsightError unable to read a file
 */
export async function getExistingDatasets(): Promise<[Map<string, number>, number]> {
	const dataPath = "./data/meta";
	const result = new Map<string, number>();
	let metaFile;
	try {
		metaFile = await readJson(dataPath);
	} catch {
		return [result, 0];
	}

	let nextName = 0;
	for (const meta of metaFile) {
		result.set(meta.id, meta.fileName);
		if (meta.fileName > nextName) {
			nextName = meta.fileName;
		}
	}
	nextName++;
	return [result, nextName];
}





