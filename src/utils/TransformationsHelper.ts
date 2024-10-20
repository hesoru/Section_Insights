import {InsightResult} from "../controller/IInsightFacade";
import {Query} from "../models/Section";

export function groupBy(filteredResults: InsightResult[], query: Query): Map<(string|number)[], InsightResult[]> | InsightResult[]{
    const groups = new Map<(string|number)[], InsightResult[]>()

    //1 Extract keys to group by
    let keys: string[] = [];
    if(query.TRANSFORMATIONS) {
        keys = query.TRANSFORMATIONS.GROUP
    } else {
        return filteredResults;
    }

    for (const result of filteredResults) {
        const values = [];
        for (const key of keys) {
            values.push(result[key]);
        }
        const mapEntry = groups.get(values);
        if(!mapEntry) {
            groups.set(values, [result]);
        } else {
            mapEntry.push(result);
        }
    }
    return groups;
}