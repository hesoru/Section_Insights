import { MKey, Query, SKey } from "../models/Section";
import { InsightError } from "../controller/IInsightFacade";

export function validateQuery(query: unknown): Query {
	// 1) Check that query is an object
	if (typeof query !== "object" || query === null) {
		throw new InsightError("invalid query, query is not a non-null object");
	}

	// 2) Check WHERE fields
	if ("WHERE" in query) {
		if (query.WHERE === null || typeof query.WHERE !== "object") {
			throw new InsightError("invalid query, query contains null WHERE field");
		}
		validateBody(query.WHERE);
	} else {
		throw new InsightError("invalid query, query does not contain WHERE field");
	}

	// 3) Check OPTION fields
	if ("OPTIONS" in query) {
		if (query.OPTIONS === null || typeof query.OPTIONS !== "object") {
			throw new InsightError("invalid query, query contains invalid OPTIONS field");
		}
		validateOptions(query.OPTIONS);
	} else {
		// throw new InsightError('invalid query, query does not contain OPTION field');
		// this is okay right
	}
	return query as Query;
}

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
			// I think this is fine because WHERE can be empty
		}
	}
}
export function validateObject(key: string, value: any, type: string): void {
	if (value === null || typeof value !== "object") {
		throw new InsightError(`invalid query, query.WHERE.${key} is invalid`);
	}
	validateComparator(value, type);
}

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

export function checkKeys(filter: any): string[] {
	const keys = Object.keys(filter);
	if (keys.length > 1) {
		throw new InsightError("invalid query, query.WHERE contains more than one key");
	}
	return keys;
}

export function validateOptions(options: any): void {
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
	for (const key of options.COLUMNS) {
		validateKey(key);
	}

	// validate order
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
}

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

function validateKey(key: any): void {
	if (typeof key !== "string") {
		throw new InsightError("invalid query, key is not a string");
	}
	if (!isMKey(key) && !isSKey(key)) {
		throw new InsightError("invalid query, key is not an Mkey or Skey");
	}
}

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
