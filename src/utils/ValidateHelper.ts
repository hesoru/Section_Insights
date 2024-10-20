import { MKey, Query, SKey } from "../models/Section";
import { InsightError } from "../controller/IInsightFacade";

/**
 * Questions:
 * is it okay to have a query without an options block?
 * for where I think our match all values scenario is not working
 * is it possible to have a transformations block without an options block?
 * Can the apply array be empty?
 * MAX/MIN/AVG/SUM should only be requested for numeric keys. COUNT can be requested for all keys.
 * If GROUP is present, all COLUMNS keys must correspond to one of the GROUP keys or to applykeys defined in the APPLY block.
 */
/**
 * @returns - Query, validates that the query param conforms to Query structure, if not throws InsightError
 * @param query
 */
export function validateQuery(query: unknown): Query {
	//1) check that query is an object
	if (typeof query !== "object" || query === null) {
		throw new InsightError("invalid query, query is not a non-null object");
	}

	//2) check WHERE fields
	if ("WHERE" in query) {
		if (query.WHERE === null || typeof query.WHERE !== "object") {
			throw new InsightError("invalid query, query contains null WHERE field");
		}
		validateBody(query.WHERE);
	} else {
		throw new InsightError("invalid query, query does not contain WHERE field");
	}

	//3) check OPTION fields
	let columnKeys
	if ("OPTIONS" in query) {
		//is the OPTIONS field allowed to be null? I don't think so
		if (query.OPTIONS === null || typeof query.OPTIONS !== "object") {
			throw new InsightError("invalid query, query contains invalid OPTIONS field");
		}
		columnKeys = validateOptions(query.OPTIONS);
	} else {
		throw new InsightError('invalid query, query does not contain OPTIONS field');
		//this is okay right
	}

	if("TRANSFORMATIONS" in query) {
		if(query.TRANSFORMATIONS === null || typeof query.TRANSFORMATIONS !== "object") {
			throw new InsightError("invalid query, query contains invalid TRANSFORMATIONS field");
		}
		validateTransformation(query.TRANSFORMATIONS, columnKeys)
	}
	return query as Query;
}

/**
 * @returns - void, if filter does not conform to query.WHERE structure throws InsightError
 * @param filter
 */
export function validateBody(filter: any): void {
	const keys = checkKeys(filter);
	const validateArray = (key: string, value: any): void => {
		if (!Array.isArray(value) || value.length === 0) {
			throw new InsightError(`invalid query, query.WHERE.${key} is invalid`);
		}
		value.forEach(validateBody);
	};
	if (keys[0] === "GT" || keys[0] === "LT" || keys[0] === "IS" || keys[0] === "EQ") {
		validateLogicComparator(keys[0], filter);
	} else {
		switch (keys[0]) {
			case "OR":
				validateArray("OR", filter.OR);
				break;
			case "AND":
				validateArray("AND", filter.AND);
				break;
			case "NOT":
				validateBody(filter.NOT);
				break;
			default:
				if (keys.length !== 0) {
					throw new InsightError("invalid query, query.WHERE contains an invalid key");
				}
			//I think this is fine because WHERE can be empty
		}
	}
}

/**
 * @returns void, checks that the key is an object, if not throws InsightError.
 * @param key
 * @param value
 * @param type
 */
export function validateObject(key: string, value: any, type: string): void {
	if (value === null || typeof value !== "object") {
		throw new InsightError(`invalid query, query.WHERE.${key} is invalid`);
	}
	validateComparator(value, type);
}

/**
 * @returns - void, validates that LogicComparator structure is correct as specified in Body interface
 * if not valid throws InsightError
 * @param key
 * @param filter
 */
export function validateLogicComparator(key: string, filter: any): void {
	switch (key) {
		case "GT":
			validateObject("GT", filter.GT, "Mkey");
			break;
		case "LT":
			validateObject("LT", filter.LT, "Mkey");
			break;
		case "EQ":
			validateObject("EQ", filter.EQ, "Mkey");
			break;
		case "IS":
			validateObject("IS", filter.IS, "SKey");
			break;
	}
}

/**
 * @returns - validates that query.WHERE contains the correct number of keys, if not throws InsightError
 * @param filter
 */
export function checkKeys(filter: any): string[] {
	const keys = Object.keys(filter);
	if (keys.length > 1) {
		throw new InsightError("invalid query, query.WHERE contains more than one key");
	}
	return keys;
}

/**
 * @returns - validates that OPTIONS portion of query conforms to Options interface, if not throws Insight Error
 * @param options
 */
export function validateOptions(options: any): Set<string> {
	const keys = Object.keys(options);
	const KEY_LENGTH = 2;
	if (keys.length === 0 || keys.length > KEY_LENGTH) {
		throw new InsightError("invalid query, query.OPTIONS incorrect number of keys");
	}
	//validate columns
	if (keys[0] !== "COLUMNS") {
		throw new InsightError("invalid query, query.OPTIONS does not contain COLUMNS");
	}
	if (!Array.isArray(options.COLUMNS) || options.COLUMNS.length === 0) {
		throw new InsightError("invalid query, query.OPTIONS.COLUMNS is not an array");
	}
	const columnsKeys = new Set<string>()
	for (const key of options.COLUMNS) {
		validateKey(key);
		columnsKeys.add(key);
	}

	//validate order
	if (keys[1]) {
		if (keys[1] !== "ORDER") {
			throw new InsightError("invalid query, query.OPTIONS does not contain ORDER as 2nd key");
		}
		if (!Object.values(options.COLUMNS).includes(options.ORDER)) {
			throw new InsightError("invalid query, query.ORDER specifies key not in OPTIONS");
		}
		{
			validateKey(options.ORDER);
		}
	}
	return columnsKeys
}

/**
 * @returns - void, validates the value found in SCompparator or MComparator, if invalid throws InsightError
 * @param comparator
 * @param field
 */
function validateComparator(comparator: [MKey, number] | [SKey, number], field: string): void {
	const keys = Object.keys(comparator);
	const values = Object.values(comparator);
	if (keys.length !== 1 || values.length !== 1) {
		throw new InsightError("invalid key in comparator");
	}

	if (field === "Mkey") {
		if (typeof values[0] !== "number" || !isMKey(keys[0])) {
			throw new InsightError("invalid Mkey in comparator");
		}
	}

	if (field === "SKey") {
		if (typeof values[0] !== "string" || !isSKey(keys[0])) {
			throw new InsightError("invalid Skey in comparator");
		}
	}
}

/**
 * @returns - validates the a given key is either a valid MKey or a valid SKey, if not throws InsightError
 * @param key
 */
function validateKey(key: any): void {
	if (typeof key !== "string") {
		throw new InsightError("invalid query, key is not a string");
	}
	if (!isMKey(key) && !isSKey(key)) {
		throw new InsightError("invalid query, key is not an Mkey or Skey");
	}
}

/**
 * @returns - validates the structure of a given MKey, checks that the field is a valid MField, if not throws InsightError
 * @param key
 */
function isMKey(key: string): boolean {
	if (!key.includes("_")) {
		return false;
	}
	const parts = key.split("_");
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(parts[0]) || parts[0].trim().length === 0) {
		return false;
	}
	const validMFields = ["avg", "pass", "fail", "audit", "year"];
	return validMFields.includes(parts[1]);
}

/**
 * @returns - validates the structure of a given SKey, checks that the field is a valid SField, if not throws InsightError
 * @param key
 */
function isSKey(key: string): boolean {
	if (!key.includes("_")) {
		return false;
	}
	const parts = key.split("_");
	const validId = /^[^_]+$/; //Adapted from chatGPT generated response.
	if (!validId.test(parts[0]) || parts[0].trim().length === 0) {
		return false;
	}
	const validSFields = ["dept", "id", "instructor", "title", "uuid"];
	return validSFields.includes(parts[1]);
}

function validateTransformation(transformations: any, columnKeys: Set<string>): void {
	const keys = Object.keys(transformations);
	const keyLength = 2;
	if(keys.length !== keyLength) {
		throw new InsightError("invalid transformation");
	}
	if(keys[0] !== "GROUP" || keys[1] !== "APPLY") {
		throw new InsightError("invalid transformation, keys are not named GROUP and APPLY");
	}
	const groupKeys = validateGroup(transformations.GROUP);
	const applyKeys = validateApply(transformations.APPLY);

	columnKeys.forEach((key) => {
		if(groupKeys.has(key) && applyKeys.has(key)) {
			throw new InsightError("all column keys do not correspond to group keys or apply keys")
		}
	})
}

function validateGroup(group: any): Set<string> {
	if(!Array.isArray(group)) {
		throw new InsightError("invalid group field within transformations, not an array");
	}
	const groupKeys = new Set<string>()
	for(const key of group) {
		validateKey(key);
		groupKeys.add(key);
	}
	return groupKeys
}
//Is an empty apply block okay?
function validateApply(apply: any): Set<string> {
	if(!Array.isArray(apply)) {
		throw new InsightError("invalid apply field in transformations");
	}
	const usedApplyKeys = new Set<string>()
	for(const applyRule of apply) {
		const newApplyKey = validateApplyRule(applyRule)
		if(usedApplyKeys.has(newApplyKey)) {
			throw new InsightError("apply key in transformations not unique");
		}
		usedApplyKeys.add(newApplyKey);
	}
	return usedApplyKeys;
}

function validateApplyRule(applyRule: any): string {
	if(typeof applyRule === "object") {
		throw new InsightError("invalid apply field in transformations, invalid applyRule found")
	}
	const keys = Object.keys(applyRule)
	if(keys.length !== 1) {
		throw new InsightError("invalid apply field in transformations, invalid applyRule found");
	}
	const validApplyKey = /^[^_]+$/;
	const applyKey = keys[0]
	if (!validApplyKey.test(applyKey)) {
		throw new InsightError("invalid apply field in transformations, invalid applyKey");
	}

	const values = Object.values(applyRule);
	if(values.length !== 1 || typeof values[0] !== "object") {
		throw new InsightError("invalid apply field in transformations, invalid applyKey value");
	}
	const applyKeyObjectKeys = Object.keys(applyRule[applyKey])
	const validApplyTokens = ['MAX', 'MIN', 'AVG', 'COUNT', 'SUM']
	if(applyKeyObjectKeys.length !== 1 || !validApplyTokens.includes(applyKeyObjectKeys[0])) {
		throw new InsightError("invalid apply field in transformations, invalid applyToken");
	}

	const applyKeyObjectValues = Object.values(applyRule[applyKey])
	if(applyKeyObjectValues.length !== 1) {
		throw new InsightError("invalid apply field in transformations, invalid applyKeyObjectValues");
	}
	validateKey(applyKeyObjectValues[0]);
	return applyKey;
}