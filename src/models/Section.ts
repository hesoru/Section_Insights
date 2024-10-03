
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
	WHERE: Where;
	OPTIONS: Options;
}

export interface Where {
	FILTER?: Filter; //filter is optional and an empty where matches all entries
}

export interface Options {

}

export type Filter = LogicComparison | MComparison | SComparison | Negation;

export interface LogicComparison {
	AND: Filter[] //Logic Comparison
	OR: Filter[]  //Logic Comparison
}

export interface MComparison {
	LT: Record<MKey, number>; //M Comparison
	GT: Record<MKey, number>; //M Comparison
	EQ: Record<MKey, number>; //M Comparison
}

export interface SComparison {
	IS: Record<SKey, string> //S Comparison
}

export interface Negation {
	NOT: Filter;  //Negation
}

export interface Options {
	COLUMNS: string[];
	ORDER?: string;  //ordering is optional
}

export type MKey = `${string}_${MField}`;
export type SKey = `${string}_${SField}`;
export type MField = 'avg' | 'pass' | 'fail' | 'audit' | 'year';
export type SField = 'dept' | 'id' | 'instructor' | 'title' | 'uuid';