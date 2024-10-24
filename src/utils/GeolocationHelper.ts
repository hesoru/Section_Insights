import { InsightError } from "../controller/IInsightFacade";

export async function getGeolocation(address: string): Promise<Geolocation> {
	//Resource: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
	const addressURI = encodeURIComponent(address);
	const url = `http://cs310.students.cs.ubc.ca:11316/api/v1/project_team154/${addressURI}`;
	try {
		const response = await fetch(url);
		if (!response.ok) {
			throw new InsightError("fetch in getGeolocation did not return a valid success code" + response.statusText);
		}
		return await response.json();
	} catch (e) {
		throw new InsightError("Failed to get Geolocation: " + e);
	}
}
