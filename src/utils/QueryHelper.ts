import {
	InsightError,
	InsightResult,
	NotFoundError
} from "../controller/IInsightFacade";
import {
	MKey, Query,
	Section,
	SKey, Body, Options
} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import {parseSectionObject} from "./JsonHelper";

/**
 * @returns - Query, validates that the query param conforms to Query structure, if not throws InsightError
 * @param query
 */
export function validateQuery(query: unknown): Query {
	//1) check that query is an object
	if(typeof query !== 'object' || query === null) {
		throw new InsightError('invalid query, query is not a non-null object');
	}

	//2) check WHERE fields
	if('WHERE' in query)  {
		if(query.WHERE === null || typeof query.WHERE !== 'object')  {
			throw new InsightError('invalid query, query contains null WHERE field');
		}
		validateBody(query.WHERE);
	} else {
		throw new InsightError('invalid query, query does not contain WHERE field');
	}

	//3) check OPTION fields
	if('OPTIONS' in query)  {
		//is the OPTIONS field allowed to be null? I don't think so
		if(query.OPTIONS === null || typeof query.OPTIONS !== 'object')  {
			throw new InsightError('invalid query, query contains invalid OPTIONS field');
		}
		validateOptions(query.OPTIONS);
	} else {
		//throw new InsightError('invalid query, query does not contain OPTION field');
		//this is okay right
	}
	return query as Query;
}

export function validateBody(filter: any): void {
	const keys = checkKeys(filter);
	const lengthLimit = 2;
	const validateArray = (key: string, value: any): void => {
		if (!Array.isArray(value) || value.length !== lengthLimit) {
			throw new InsightError(`invalid query, query.WHERE.${key} is invalid`);
		}
		value.forEach(validateBody);
	};

	const validateObject = (key: string, value: any, type: string): void => {
		if (value === null || typeof value !== 'object') {
			throw new InsightError(`invalid query, query.WHERE.${key} is invalid`);
		}
		validateComparator(value, type);
	};

	switch (keys[0]) {
		case 'OR': validateArray('OR', filter.OR);
			break;
		case 'AND': validateArray('AND', filter.AND);
			break;
		case 'GT': validateObject('GT', filter.GT, "Mkey");
			break;
		case 'LT': validateObject('LT', filter.LT, "Mkey");
			break;
		case 'EQ': validateObject('EQ', filter.EQ, "Mkey");
			break;
		case 'IS': validateObject('IS', filter.IS, "SKey");
			break;
		case 'NOT': validateBody(filter.NOT);
			break;
		default: throw new InsightError('invalid query, query.WHERE contains an invalid key');
	}
}

// export function validateBody(filter: any): void {
// 	const keys = checkKeys(filter);
// 	const lengthLimit = 2
// 	//checks type of each possible filter stopping at Mkey and Skey
// 	switch(keys[0]) {
// 		case 'OR': if(filter.OR === null || !Array.isArray(filter.OR) || filter.OR.length !== lengthLimit) {
// 			throw new InsightError('invalid query, query.WHERE.OR is invalid') }
// 			for(const body of filter.OR) {
// 				validateBody(body); }
// 			break;
// 		case 'AND': if(filter.AND === null || !Array.isArray(filter.AND) || filter.AND.length !== lengthLimit) {
// 			throw new InsightError('invalid query, query.WHERE.AND is invalid') }
// 			for(const body of filter.AND) {
// 				validateBody(body); }
// 			break;
// 		case 'GT': if(filter.GT === null || typeof filter.GT !== 'object')  {
// 			throw new InsightError('invalid query, query.WHERE.GT is invalid') }
// 			validateComparator(filter.GT, "Mkey");
// 			break;
// 		case 'LT': if(filter.LT === null || typeof filter.LT !== 'object') {
// 			throw new InsightError('invalid query, query.WHERE.LT is invalid') }
// 			validateComparator(filter.LT, "Mkey");
// 			break;
// 		case 'EQ': if(filter.EQ === null || typeof filter.EQ !== 'object') {
// 			throw new InsightError('invalid query, query.WHERE.EQ is invalid') }
// 			validateComparator(filter.EQ, "Mkey");
// 			break;
// 		case 'IS': if(filter.IS === null || typeof filter.IS !== 'object'){
// 			throw new InsightError('invalid query, query.WHERE.IS is invalid') }
// 			validateComparator(filter.IS, "SKey");
// 			break;
// 		case 'NOT': if(filter.NOT === null || typeof filter.NOT !== 'object') {
// 			throw new InsightError('invalid query, query.WHERE.NOT is invalid') }
// 			validateBody(filter.NOT);
// 			break;
// 		default: throw new InsightError('invalid query, query.WHERE contains an invalid key'); }
// }

export function checkKeys(filter: any): string[] {
	const keys = Object.keys(filter);
	if(keys.length !== 1) {
		throw new InsightError('invalid query, query.WHERE contains more than one key'); }
	return keys;
}

export function validateOptions(options: any): void {
	const keys = Object.keys(options);
	const KEY_LENGTH = 2;
	if(keys.length === 0 || keys.length > KEY_LENGTH) {
		throw new InsightError('invalid query, query.OPTIONS incorrect number of keys');
	}
	//validate columns
	if(keys[0] !== 'COLUMNS') {
		throw new InsightError('invalid query, query.OPTIONS does not contain COLUMNS');
	}
	if(!Array.isArray(options.COLUMNS) || options.COLUMNS.length === 0) {
		throw new InsightError('invalid query, query.OPTIONS.COLUMNS is not an array');
	}
	for(const key of options.COLUMNS) {
		validateKey(key);
	}

	//validate order
	if(keys[1]) {
		if (keys[1] !== 'ORDER') {
			throw new InsightError('invalid query, query.OPTIONS does not contain ORDER as 2nd key');
		}
		if (!Object.values(options.COLUMNS).includes(options.ORDER)) {
			throw new InsightError('invalid query, query.ORDER specifies key not in OPTIONS')
		}
		{validateKey(options.ORDER);}
	}

}

function validateComparator(comparator: [MKey, number] | [SKey, number], field: string): void {
	const keys = Object.keys(comparator);
	const values = Object.values(comparator);
	if(keys.length !== 1 || values.length !== 1) {
		throw new InsightError('invalid key in comparator');
	}

	if(field === "Mkey") {
		if(typeof values[0] !== 'number' || !isMKey(keys[0])) {
			throw new InsightError('invalid Mkey in comparator');
		}
	}

	if(field === "SKey") {
		if(typeof values[0] !== 'string' || !isSKey(keys[0])) {
			throw new InsightError('invalid Skey in comparator');
		}
	}
}

function validateKey(key: any): void {
	if(typeof key !== 'string') {
		throw new InsightError('invalid query, key is not a string');
	}
	if(!isMKey(key) && !isSKey(key)) {
		throw new InsightError('invalid query, key is not an Mkey or Skey')
	}
}

function isMKey(key: string): boolean {
	if (!key.includes('_')) {
		return false;
	}
	const parts = key.split('_');
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(parts[0]) || parts[0].trim().length === 0) {
		return false;
	}
	const validMFields = ['avg', 'pass', 'fail', 'audit', 'year'];
	return validMFields.includes(parts[1]);
}

function isSKey(key: string): boolean {
	if (!key.includes('_')) {
		return false;
	}
	const parts = key.split('_');
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(parts[0]) || parts[0].trim().length === 0) {
		return false;
	}
	const validSFields = ['dept', 'id', 'instructor', 'title', 'uuid'];
	return validSFields.includes(parts[1]);
}

export function handleFilter(filter: Body, data: InsightResult[]): InsightResult[] {
	if (!filter) {
		return data;
	}
	if (filter.AND || filter.OR) {
		return handleLogicComparison(filter, data);
	} else if (filter.LT || filter.GT || filter.EQ) {
		return handleMComparison(filter, data);
	} else if (filter.IS) {
		return handleSComparison(filter, data);
	} else if (filter.NOT) {
		return handleNegation(filter, data);
	} else {
		throw new InsightError('Invalid filter');
	}
}

function handleLogicComparison(filter: Body, data: InsightResult[]): InsightResult[] {  //return array of insight result.
	let results: InsightResult[] = [];

	if (filter.AND) {
		// intersection of all results: all filters must be true
		// return filter.AND.reduce((acc, subBody) => handleBody(subBody, acc), data);
		for (const subFilter of filter.AND) {
			const subResults = handleFilter(subFilter, data);
			if(results.length === 0) {
				results = subResults;
			} else {
				results = results.filter(section => subResults.includes(section)); // Intersection of results
			}
		}
	} else if (filter.OR) {
		for (const subFilter of filter.OR) {
			const subResults = handleFilter(subFilter, data);
			results = results.concat(subResults); // Union of results
		}
		results = Array.from(new Set(results)); // Remove duplicates

	} else {
		throw new InsightError('Invalid logic filter');
	}
	return results;
}

function handleMComparison(filter: any, data: InsightResult[]): InsightResult[] {
	if (filter.GT) {
		const mKey = Object.keys(filter.GT)[0];
		const parts = mKey.split('_');
		const field = parts[1];
		const value = Object.values(filter.GT)[0];
		return data.filter((section) => section[field] > (value as number));
	}
	if (filter.LT) {
		const mKey = Object.keys(filter.LT)[0];
		const parts = mKey.split('_');
		const field = parts[1];
		const value = Object.values(filter.LT)[0];
		return data.filter((section) => section[field] < (value as number));
	}
	if (filter.EQ) {
		const mKey = Object.keys(filter.EQ)[0];
		const parts = mKey.split('_');
		const field = parts[1];
		const value = Object.values(filter.EQ)[0];
		return data.filter((section) => section[field] === (value as number));
	}
	throw new InsightError("Invalid MComparator operator.")
}

function handleSComparison(filter: any, data: InsightResult[]): InsightResult[] {
	const [sKey, value] = filter.IS;
	const regex = new RegExp(`^${value.replace(/\*/g, ".*")}$`); // Handle wildcards
	return data.filter((section) => regex.test(section[sKey] as string));
}

function handleNegation(filter: any, data: InsightResult[]): InsightResult[] {
	// Exclude matching sections
	const notData = handleFilter(filter.NOT, data);
	return data.filter((section) => !notData.includes(section));
}

export async function getAllSections(query: Query): Promise<InsightResult[]> {
	const idString = extractDatasetId(query);
	const allSections = await loadDatasets(idString, 'sections');

	const columns = Object.keys(allSections[0]);

	const allResults: InsightResult[] = [];
	for (const section of allSections) {
		const sectionResult: InsightResult = {};
		for (const item of columns) {  //iterate through indicies
			sectionResult[item] = section[item as keyof Section];
		}
		allResults.push(sectionResult);
	}

	return allResults;
}

export function sortResults(options: Options, results: InsightResult[]): InsightResult[] {
	return results.sort((a, b) => {
		const aValue = a[options.ORDER as string | number];
		const bValue = b[options.ORDER as string | number];

		if (typeof aValue === 'string' && typeof bValue === 'string') {
			// string comparison (case-insensitive)
			return aValue.localeCompare(bValue, undefined, { sensitivity: 'base' });
		}

		if (typeof aValue === 'number' && typeof bValue === 'number') {
			// numeric comparison
			return aValue - bValue;
		}

		// types differ
		throw new InsightError("Comparison column contains strings and numbers together!")
	});
}

export function extractDatasetId(query: Query): string {
	const key = extractKey(query.WHERE);
	const keyParts = key.split('_');
	return keyParts[0];
}

export function extractKey(body: Body): string {
	if(body.EQ) {
		return Object.keys(body.EQ)[0];
	} else if(body.GT) {
		return Object.keys(body.GT)[0];
	} else if(body.LT) {
		return Object.keys(body.LT)[0];
	}

	if(body.AND) {
		for (const b of body.AND) {
			const key = extractKey(b);
			if(key) {
				return key;}
		}
	} else if(body.OR) {
		for(const b of body.OR) {
			const key = extractKey(b);
			if(key) {
				return key;}
		}
	} else if(body.NOT) {
		return extractKey(body.NOT);
	}
	throw new InsightError("Invalid body");
}

//THIS IS THE HELPER TO CALL IN EACH BASE CASE FOR WHERE FUNCTION: MCOMPARATOR, SCOMPARATOR
// /**
//  * @returns - Section[] of sections returned by loadDatasets contained in the data set specified in the id string of
//  * param field. Sections have been filtered according to op and Mfield|Sfield.
//  * @param op
//  * @param record
//  */
// export async function getMatchingSections(op: string, record: [MKey, number] | [SKey, string]): Promise<Section[]> {
//
// 	const key = record[0];
// 	const value = record[1];
// 	const parts = key.split('_');
// 	const idString = parts[0];
// 	const field = parts[1];
//
// 	//Adapted form ChatGPT generated response
// 	const comp = generateCompFunction(op);
//
// 	const sections = await loadDatasets(idString, 'sections');
// 	const results: Section[] = [];
// 	for (const section of sections) {
// 		if (filterSection(section, comp, value, field)) {
// 			results.push(section);
// 		}
// 	}
// 	return results;
// }
//
// export function generateCompFunction(op: string): CompFunction {
// 	switch(op) {
// 		case 'GT':
// 			return ((a: number, b: number) => a > b) as CompFunction;
// 		case 'LT':
// 			return ((a: number, b: number) => a < b) as CompFunction;
// 		case 'EQ':
// 			return ((a: number, b: number) => a === b) as CompFunction;
// 		case 'IS':
// 			return ((a: string, b: string) => a === b) as CompFunction;
// 		default:
// 			throw new InsightError('not good very bad, no teneís un Quixote')
// 	}
// }
//
// export function filterSection(section: Section, comp: CompFunction, value: string | number, field: string): boolean {
// 	switch(field) {
// 		case 'avg':
// 			return comp(section.avg, value);
// 		case 'pass':
// 			return comp(section.pass, value);
// 		case 'fail':
// 			return comp(section.fail, value);
// 		case 'audit':
// 			return comp(section.audit, value);
// 		case 'year':
// 			return comp(section.year, value);
// 		case 'dept':
// 			return comp(section.dept, value);
// 		case 'id':
// 			return comp(section.id, value);
// 		case 'instructor':
// 			return comp(section.instructor, value);
// 		case 'title':
// 			return comp(section.title, value);
// 		case 'uuid':
// 			return comp(section.uuid, value);
// 		default:
// 			throw new InsightError('invalid key passed to getMatchingSections');
// 	}
// }

/**
 * @returns - Promise<Section[]> an array of the sections contained in the data specified by param id.
 * If dataset with name data/fileName can not be found throws NotFoundError
 * @param fileName
 * @param id
 */
export async function loadDatasets(id: string, fileName: string): Promise<Section[]> {
	// const MAX_SIZE = 5000;
	const datasetPath = path.resolve(__dirname, "../data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath)
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error)
	}
	const parsedSections: Section[] = [];
	for(const file of dataset) {
		for (const section of file.result) {
			const newSection = parseSectionObject(section);
			parsedSections.push(newSection);
			// WANT TO RETURN ALL SECTIONS
			// if(parsedSections.length > MAX_SIZE) {
			// 	throw new ResultTooLargeError('result exceeded 5000 sections');
			// }
		}
	}
	return parsedSections;
}


/**
 * @returns - InsightResult[], selects columns from array of sections as specified by options.COLUMNS and parses them
 * into InsightResult. Creates one Insight Result per section. Final InsightResult[] ordered according to column specified
 * by options.ORDER if applicable.
 * @param options
 * @param sections
 */
// export function handleOptions(options: Options, sections: Section[]): InsightResult[] {
// 	const columns = options.COLUMNS;
//
// 	const column_keys: string[] = [];
// 	for (const column of columns) {
// 		if (!isMKey(column) && !isSKey(column)) {
// 			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
// 		}
// 		const parts = column.split('_');
// 		column_keys.push(parts[1]);
// 	}
//
// 	const insight_results: InsightResult[] = [];
// 	for (const section of sections) {
// 		const section_result: InsightResult = {};
// 		for (let i = 0; i < column_keys.length; i++) {  //iterate through indices
// 			section_result[columns[i]] = section[column_keys[i] as keyof Section]; //WILL THIS LINE WORK? PROBABLY BUG HERE
// 		}
// 		insight_results.push(section_result);
// 	}
//
// 	if (options.ORDER) {
// 		const order = options.ORDER;
// 		if (!isMKey(order) && !isSKey(order)) {
// 			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY');
// 		}
// 		const parts = order.split('_');
// 		const field = parts[1];
//
// 		//Adapted from ChatGPT generated response:
// 		insight_results.sort((a, b) => {
// 			const valueA = a[field];
// 			const valueB = b[field];
//
// 			if (typeof valueA === 'number' && typeof valueB === 'number') {
// 				return valueA - valueB;
// 			} else if (typeof valueA === 'string' && typeof valueB === 'string') {
// 				return valueA.localeCompare(valueB, undefined, {sensitivity: 'base'});
// 			} else {
// 				throw new InsightError('fields in handleOptions.ORDER not of same type.')
// 			}
// 		})
//
// 	}
// 	return insight_results;
// }

