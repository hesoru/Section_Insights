import { InsightDatasetKind } from "../controller/IInsightFacade";

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

export interface Meta {
	id: string;
	fileName: number;
	kind: InsightDatasetKind;
	numRows: number;
}

/**
 * Types to represent specific fields in Body interface in accordance with InsightFacade EBNF
 */
export type SectionMKey = `${string}_${SectionMField}`; //adapted from ChatGPT generated response
export type SectionSKey = `${string}_${SectionSField}`; //adapted from ChatGPT generated response
export type SectionMField = "avg" | "pass" | "fail" | "audit" | "year";
export type SectionSField = "dept" | "id" | "instructor" | "title" | "uuid";

// interface GeoResponse {
// 	lat?: number;
// 	lon?: number;
// 	error?: string;
// }

// 'lat' | 'lon' | 'seats' | 'fullname' | 'shortname' | 'number' | 'name' | 'address' | 'type' | 'furniture' | 'href'
