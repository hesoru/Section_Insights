/**
 * Interface to represent the data of a course section in memory. Queries will be checked against Section objects.
 * Fields will be populated from JSON file fields according to specification:
 * File -> Dataset
 * id -> uuid
 * Course -> id
 * Title -> title
 * Professor -> instructor
 * Subject -> dept
 * Year -> year
 * Avg -> avg
 * Pass -> pass
 * Fail -> fail
 * Audit -> audit
 */
export interface Section {
	readonly uuid: string; //can not be changed when set, unique identifier for the section
	id: string; //course identifier
	title: string;
	instructor: string;
	dept: string;
	year: number;
	avg: number;
	pass: number;
	fail: number;
	audit: number;
}

/**
 * Interface to represent the JSON object created by JSON.parse(), contains all queryable files as specified in the
 * inputted file
 */
export interface JSONFile {
	id: string;
	Course: string;
	Title: string;
	Professor: string;
	Subject: string;
	Year: number;
	Avg: number;
	Pass: number;
	Fail: number;
	Audit: number;
	Section?: string;
}

/**
 * Interface to represent a Query, structure of inputted query must be previously validated by validateQuery()
 */
export interface Query {
	WHERE: Body;
	OPTIONS: Options;
}

/**
 * Interface to represent the WHERE portion of a Query, must be present in every valid query. Specifies which sections
 * are included in InsightResult[]
 */
export interface Body {
	AND?: Body[];
	OR?: Body[];
	GT?: [MKey, number];
	LT?: [MKey, number];
	EQ?: [MKey, number];
	IS?: [SKey, string];
	NOT?: Body;
}

/**
 * Interface to represent the OPTIONS portion of a query, may be present or absent from Query. Specifies which sections
 * are included in the InsightResult[] and in what order
 */
export interface Options {
	COLUMNS: string[];
	ORDER?: string;
}

/**
 * Types to represent specific fields in Body interface in accordance with InsightFacade EBNF
 */
export type MKey = `${string}_${MField}`; //adapted from ChatGPT generated response
export type SKey = `${string}_${SField}`; //adapted from ChatGPT generated response
export type MField = "avg" | "pass" | "fail" | "audit" | "year";
export type SField = "dept" | "id" | "instructor" | "title" | "uuid";
