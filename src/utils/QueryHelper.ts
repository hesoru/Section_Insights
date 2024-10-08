import {InsightError, InsightResult, NotFoundError} from "../controller/IInsightFacade";
import {
	MKey, Query,
	Section,
	SKey, Options, CompFunction
} from "../models/Section";
import fs from "fs-extra";
import path from "node:path";
import {parseJSONtoSections} from "./JsonHelper";

export async function processQueryOnDataset(query: unknown): Promise<InsightResult[]> {
	//let validatedQuery: Query;
	try {
		//validatedQuery = validateQuery(query)
		validateQuery(query);

	} catch (error) {
		throw new Error(`Query not a valid format: ` + error);
	}
	//const allMatchingSections: Section[] = handleBody(validatedQuery.WHERE);  //should return InsightResult[] that matches Body specifications
	//const orderedMatchingSections = handleOptions(validatedQuery.OPTIONS, allMatchingSections); //should return same InsightResult[] but sorted to Option specifications
	return [];
}


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
	const keys = Object.keys(filter);
	if(keys.length !== 1) {
		throw new InsightError('invalid query, query.WHERE contains more than one key'); }

	//checks type of each possible filter stopping at Mkey and Skey
	switch(keys[0]) {
		case 'OR': if(filter.OR === null || !Array.isArray(filter.OR) || filter.OR.length !== 2) {
				throw new InsightError('invalid query, query.WHERE.OR is invalid') }
			for(const body of filter.OR) {
				validateBody(body); }
			break;
		case 'AND': if(filter.AND === null || !Array.isArray(filter.AND) || filter.AND.length !== 2) {
				throw new InsightError('invalid query, query.WHERE.AND is invalid') }
			for(const body of filter.AND) {
				validateBody(body); }
			break;
		case 'GT': if(filter.GT === null || typeof filter.GT !== 'object')  {
				throw new InsightError('invalid query, query.WHERE.GT is invalid') }
			validateComparator(filter.GT, "Mkey");
			break;
		case 'LT': if(filter.LT === null || typeof filter.LT !== 'object') {
				throw new InsightError('invalid query, query.WHERE.LT is invalid') }
			validateComparator(filter.LT, "Mkey");
			break;
		case 'EQ': if(filter.EQ === null || typeof filter.EQ !== 'object') {
				throw new InsightError('invalid query, query.WHERE.EQ is invalid') }
			validateComparator(filter.EQ, "Mkey");
			break;
		case 'IS': if(filter.IS === null || typeof filter.IS !== 'object'){
				throw new InsightError('invalid query, query.WHERE.IS is invalid') }
			validateComparator(filter.IS, "SKey");
			break;
		case 'NOT': if(filter.NOT === null || typeof filter.NOT !== 'object') {
				throw new InsightError('invalid query, query.WHERE.NOT is invalid') }
			validateBody(filter.NOT);
			break;
		default: throw new InsightError('invalid query, query.WHERE contains an invalid key'); }
}

export function validateOptions(options: any): void {
	const keys = Object.keys(options);
	if(keys.length === 0 || keys.length > 2) {
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

//SPACE FOR HELENA FUNCTIONS


//THIS IS THE HELPER TO CALL IN EACH BASE CASE FOR WHERE FUNCTION: MCOMPARATOR, SCOMPARATOR
/**
 * @returns - Section[] of sections returned by loadDatasets contained in the data set specified in the id string of
 * param field. Sections have been filtered according to op and Mfield|Sfield.
 * @param op
 * @param record
 */
export async function getMatchingSections(op: string, record: [MKey, number] | [SKey, string]): Promise<Section[]> {

	const key = record[0];
	const value = record[1];
	const parts = key.split('_');
	const idString = parts[0];
	const field = parts[1];

	//Adapted form ChatGPT generated response
	const comp = generateCompFunction(op);

	const sections = await loadDatasets(idString, 'sections');
	const results: Section[] = [];
	for (const section of sections) {
		if (filterSection(section, comp, value, field)) {
			results.push(section);
		}
	}
	return results;
}

export function generateCompFunction(op: string): CompFunction {
	switch(op) {
		case 'GT':
			return ((a: number, b: number) => a > b) as CompFunction;
		case 'LT':
			return ((a: number, b: number) => a < b) as CompFunction;
		case 'EQ':
			return ((a: number, b: number) => a === b) as CompFunction;
		case 'IS':
			return ((a: string, b: string) => a === b) as CompFunction;
		default:
			throw new InsightError('not good very bad, no teneís un Quixote')
	}
}

export function filterSection(section: Section, comp: CompFunction, value: string | number, field: string): boolean {
	switch(field) {
		case 'avg':
			return comp(section.avg, value);
		case 'pass':
			return comp(section.pass, value);
		case 'fail':
			return comp(section.fail, value);
		case 'audit':
			return comp(section.audit, value);
		case 'year':
			return comp(section.year, value);
		case 'dept':
			return comp(section.dept, value);
		case 'id':
			return comp(section.id, value);
		case 'instructor':
			return comp(section.instructor, value);
		case 'title':
			return comp(section.title, value);
		case 'uuid':
			return comp(section.uuid, value);
		default:
			throw new InsightError('invalid key passed to getMatchingSections');
	}
}

/**
 * @returns - Promise<Section[]> an array of the sections contained in the data specified by param id.
 * If dataset with name data/fileName can not be found throws NotFoundError
 * @param fileName
 * @param id
 */
export async function loadDatasets(id: string, fileName: string): Promise<Section[]> {
	const datasetPath = path.resolve(__dirname, "../data", fileName);
	let dataset;
	try {
		dataset = await fs.readJson(datasetPath)
	} catch (error) {
		throw new NotFoundError(`Could not find dataset with - id=${id};` + error)
	}

	return parseJSONtoSections(dataset);
}


/**
 * @returns - InsightResult[], selects columns from array of sections as specified by options.COLUMNS and parses them
 * into InsightResult. Creates one Insight Result per section. Final InsightResult[] ordered according to column specified
 * by options.ORDER if applicable.
 * @param options
 * @param sections
 */
export function handleOptions(options: Options, sections: Section[]): InsightResult[] {
	const columns = options.COLUMNS;

	const columnKeys: string[] = [];
	for(const column of columns) {
		if(!isMKey(column) && !isSKey(column)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY'); }
		const parts = column.split('_');
		columnKeys.push(parts[1]);
	}

	const insightResults: InsightResult[] = [];
	for(const section of sections) {
		const sectionResult: InsightResult = {};
		for (let i = 0; i < columnKeys.length; i++) {  //iterate through indicies
			sectionResult[columns[i]] = section[columnKeys[i] as keyof Section]; //WILL THIS LINE WORK? PROBABLY BUG HERE
		}
		insightResults.push(sectionResult); }

	if (options.ORDER) {
		const order = options.ORDER;
		if(!isMKey(order) && !isSKey(order)) {
			throw new InsightError('invalid keys passed to OPTIONS.COLUMNS, NOTE THIS WAS NOT CAUGHT IN VALIDATE QUERY'); }
		const parts = order.split('_');
		const field = parts[1];

		//Adapted from ChatGPT generated response:
		insightResults.sort((a, b) => {
			const valueA = a[field];
			const valueB = b[field];

			if(typeof valueA === 'number' && typeof valueB === 'number') {
				return valueA - valueB;
			} else if(typeof valueA === 'string' && typeof valueB === 'string') {
				return valueA.localeCompare(valueB, undefined, {sensitivity: 'base'});
			} else {
				throw new InsightError('fields in handleOptions.ORDER not of same type.') }
		})

	}
	return insightResults;
}

