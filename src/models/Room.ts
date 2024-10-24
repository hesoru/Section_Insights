/**
 * addDataset - content: file is in the format of a base64 string.
 * Room data is contained in .htm files, spread between index.htm
 * and ACC.htm(building htm file)
 * Root of the zip is the index.htm file.
 * A valid dataset is a zip file with at least one valid room.
 * A valid index.htm is a html formattted file, if it exists can assume it is correctly formatted
 * Contains a table that lists and links to building files. It can contain many tables but only one will be the valid
 * building list table.
 * No table is invalid.
 *
 * A valid building file: is a html formatted file
 * contains a table with valid rooms, same thing file cna contain many tables
 * but only one will be the valid room table.
 * A building file with no rooms in invalid.
 * A valid room contains every field which can be used to query:
 * All valid index.htm tables will have views-field and views-field-field-building-address
 * Fine the valid rooms table by finding the table with the valid classes.
 * The validate the table.
 *
 * Geolocation:
 * For a building with a valid room eed to fetch the building long and lat.
 * Must send a get request to http://cs310.students.cs.ubc.ca:11316/api/v1/project_team<TEAM NUMBER>/<ADDRESS>
 * Address must be URL encoded version of the address.
 * Must only query the service when processing initial dataset zips not when answering queries.
 */
//
// interface GeoResponse {
//     lat?: number;
//     lon?: number;
//     error?: string;
// }

//why do lat and lon here have? Shouldn't we always have them?
export interface Building {
	fullname: string;
	shortname: string;
	address: string;
	lat?: number;
	lon?: number;
	href: string;
}

export interface Room {
	building: Building;
	name: string;
	number: string; // not always a number so represented as a string
	type: string;
	furniture: string;
	seats: number;
}

//Why do we have the address in GeoResponse? is it not the return type they specified in the specs?
interface GeoResponse {
	address: string;
	lat?: number;
	lon?: number;
	error?: string;
}

// 'lat' | 'lon' | 'seats' | 'fullname' | 'shortname' | 'number' | 'name' | 'address' | 'type' | 'furniture' | 'href'
