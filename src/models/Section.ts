
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
}

export interface Query {
	WHERE: Body;
	OPTIONS?: Options;
}

export interface Body {
	AND?: Body[];
	OR?: Body[];
	GT?: Record<MField, number>;
	LT?: Record<MField, number>;
	EQ?: Record<MField, number>;
	IS?: Record<SField, number>;
	NOT?: Body;
}

interface Options {
	COLUMNS: string[];
	ORDER?: string;
}

export type MKey = `${string}_${MField}`;
export type SKey = `${string}_${SField}`;
export type MField = 'avg' | 'pass' | 'fail' | 'audit' | 'year';
export type SField = 'dept' | 'id' | 'instructor' | 'title' | 'uuid';