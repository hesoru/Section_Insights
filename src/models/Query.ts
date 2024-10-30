import { SectionMKey, SectionSKey } from "./Section";
import { RoomMKey, RoomSKey } from "./Room";

/**
 * Interface to represent a Query, structure of inputted query must be previously validated by validateQuery()
 */
export interface Query {
	WHERE: Body;
	OPTIONS: Options;
	TRANSFORMATIONS: Transformations;
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
	ORDER?: string | Order;
}

export interface Order {
	dir: Dir;
	keys: string[];
}

export interface Transformations {
	GROUP: string[];
	APPLY?: ApplyRule[];
}

export type ApplyRule = Record<string, ApplyTokenObject>;

export interface ApplyTokenObject {
	MIN?: MKey | SKey;
	MAX?: MKey | SKey;
	AVG?: MKey | SKey;
	COUNT?: MKey | SKey;
	SUM?: MKey | SKey;
}

export type MKey = SectionMKey | RoomMKey;
export type SKey = SectionSKey | RoomSKey;
export type ApplyToken = "MIN" | "MAX" | "AVG" | "COUNT" | "SUM";
export type Dir = "UP" | "DOWN";
