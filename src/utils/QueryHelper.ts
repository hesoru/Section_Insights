import { InsightResult } from "../controller/IInsightFacade";

export function validateQuery(query: unknown): boolean {
	return true;
}

export function extractDatasetId(query: unknown): string {
	return "";
}

export async function processQueryOnDataset(query: unknown, id: string): Promise<InsightResult[]> {
	return [];
}
