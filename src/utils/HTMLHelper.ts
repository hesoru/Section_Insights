import parse5, {DefaultTreeAdapterMap} from 'parse5';
import fs from 'fs';
import JSZip from "jszip";
import {InsightDataset, InsightError} from "../controller/IInsightFacade";
import {unzipContent} from "./JsonHelper";
import {Building, Room} from "../models/Section";
import { Document } from 'parse5/dist/tree-adapters/default';
import * as http from "node:http";

export async function parseIndexString(indexString: string): Promise<Building[]> {
	try {
		// parse index HTML into document (node tree)
		const document = parse5.parse(indexString);
		// find buildings table in document and extract buildings from it
		const tableBodyNode = findTableBodyNode(document);
		const buildingsIndex = extractBuildingsIndex(tableBodyNode);
		// add geolocation data to all buildings
		return await addGeolocationDataToAll(buildingsIndex);
	} catch (error) {
		throw new InsightError("Failed to extract buildings from index file: " + error);
	}
}

export function parseBuildingStrings(buildingStrings: string[], buildingsIndex: Building[]): Room[] {
	const roomsDataset: Room[] = [];
	try {
		// for each rooms HTML file:
		buildingStrings.forEach(file => {
			// parse rooms HTML into document (node tree)
			const document = parse5.parse(file);
			const buildingString = extractBuildingString(document);
			// find rooms table in document and extract rooms from it
			const tableBodyNode = findTableBodyNode(document);
			const roomsData = extractRooms(tableBodyNode);
			// match building string to building in index
			const buildingRooms = addBuildingToRooms(buildingsIndex, buildingString, roomsData);
			// push rooms for each building to roomsDataset
			buildingRooms.forEach(room => {
				roomsDataset.push(room);
			});
		});
		return roomsDataset;
	} catch (error) {
		throw new InsightError("Failed to extract rooms from building file: " + error);
	}
}

async function addGeolocationDataToAll(buildingsIndex: Building[]): Promise<Building[]> {
	try {
		// for each building in buildingsIndex:
		return await Promise.all(buildingsIndex.map(async (building) => {
			// encode the address in URL format
			const addressURL = encodeURIComponent(building.address);
			try {
				// add geolocation data to building
				return await addGeolocationData(building, addressURL);
			} catch {
				// TODO: what do we do if we don't receive geolocation data? reject the building?
				// return building without geolocation data otherwise
				return building;
			}
		}));
	} catch (error) {
		throw new InsightError("Adding geolocation data failed: " + error);
	}
}

async function addGeolocationData(building: Building, addressURL: string): Promise<Building> {
	return new Promise((resolve, reject) => {
		const url = `http://cs310.students.cs.ubc.ca:11316/api/v1/project_team154/${addressURL}`;
		let geoResponse = null;

		// send HTTP GET request
		http.get(url, (res) => {
			let data = '';
			// collect response data in chunks
			res.on('data', (chunk) => {
				data += chunk;
			});
			// once request is complete:
			res.on('end', () => {
				try {
					// parse JSON-formatted georesponse into object
					geoResponse = JSON.parse(data);
					// if georesponse does not contain an error, transfer lat/lon into requested building
					if (!geoResponse.error) {
						building.lat = geoResponse.lat;
						building.lon = geoResponse.lon;
					}
					resolve(building);  // resolve promise
				} catch (error) {
					reject(error);  // reject promise
				}
			});
		}).on('error', (error) => {
			reject(error);  // reject promise
		});
	});
}

// match building to roomsData based on given buildingString (from roomsData)
function addBuildingToRooms(buildingsIndex: Building[], buildingString: string, roomsData: Room[]): Room[] {
	let foundBuilding = {} as Building;
	try {
		// every() behaves like foreach(), except stops iterating when receiving a false value
		buildingsIndex.every((building) => {
			if (building.fullname === buildingString) {
				foundBuilding = building;
				if (foundBuilding) {
					return false;
				}
			}
			return true;
		});
		if (!foundBuilding) {
			// invalid dataset - return empty array
			return [];
		}
		// else add building to every room in roomsData
		roomsData.forEach((room) => {
			room.building = foundBuilding;
		})
		return roomsData;
	} catch (error) {
		throw new InsightError("Not able to find building in index file that matches building in rooms file: " + error);
	}
}

// recursive function to traverse the document tree and extract table rows
export function findTableBodyNode(node: Element): { nodeName: string; attrs: any[]; childNodes: any; } {
	// verify if node is a table node (return tbody node)
	if (isElementNode(node) && node.nodeName === 'table' && node.childNodes) {
		// const tableNode = node.childNodes.find((child: any) => child.nodeName === 'tbody');
		for (const child in node.childNodes) {
			if (child.nodeName === 'tbody') {
				return child;
			}
		}
		// node.childNodes.forEach(child => {
		// 	if (child.nodeName === 'tbody') {
		// 		return child;
		// 	}
		// })
		// const isTableNode = node.attrs.some(attr => attr.value.includes('views-table cols-5 table'));
		// if (isTableNode) {
		// 	return node.childNodes.find(child => child.nodeName === 'tbody');
		// }
	}

	// recurse through tree to find table
	let foundTableBodyNode = null;
	if (node.childNodes) {
		for (const child of node.childNodes) {
			foundTableBodyNode = findTableBodyNode(child);
			if (foundTableBodyNode) {
				return foundTableBodyNode;
			}
		}
	}

	// throw error if table not found in tree
	throw new InsightError("Data table not found in HTML file!")
}

function extractBuildingsIndex(node: Node): Building[] {
	const buildingsIndex: Building[] = [];
	if (node.nodeName === 'tr' && node.childNodes) {
		const building = {} as Building;
		node.childNodes.forEach((child: {nodeName: string; attrs: any[]; childNodes: {value: string;}[];}) => {
			if (child.nodeName === 'td') {
				if (child.attrs?.some((attr: any) => attr.name === 'class'
					&& attr.value.includes('views-field-field-building-code'))) {
					building.shortname = child.childNodes[0].value.trim();
				}
				if (child.attrs?.some((attr: any) => attr.name === 'class'
					&& attr.value.includes('views-field-title'))) {
					building.fullname = child.childNodes[0].childNodes[0].value.trim();
					building.href = child.childNodes[0].attrs.find(attr => attr.name === 'href').value;
				}
				if (child.attrs?.some((attr: any) => attr.name === 'class'
					&& attr.value.includes('views-field-field-building-address'))) {
					building.address = child.childNodes[0].value.trim();
				}
			}
		});
		buildingsIndex.push(building);
	}
	// recurse through tree
	if (node.childNodes) {
		node.childNodes.forEach((child: any) => extractBuildingsIndex(child));
	}
	return buildingsIndex;
}

function extractRooms(node: any): Room[] {
	const rooms: Room[] = [];
	if (node.nodeName === 'tr' && node.childNodes) {
		const room = {} as Room;
		node.childNodes.forEach(child => {
			if (child.nodeName === 'td') {
				if (child.attrs.some(attr => attr.name === 'class'
					&& attr.value.includes('views-field-field-room-number'))) {
					room.number = child.childNodes[0].childNodes[0].value.trim();
				}
				if (child.attrs.some(attr => attr.name === 'class'
					&& attr.value.includes('views-field-field-room-capacity'))) {
					room.seats = child.childNodes[0].value.trim();
				}
				if (child.attrs.some(attr => attr.name === 'class'
					&& attr.value.includes('views-field-field-room-furniture'))) {
					room.furniture = child.childNodes[0].value.trim();
				}
				if (child.attrs.some(attr => attr.name === 'class'
					&& attr.value.includes('views-field-field-room-type'))) {
					room.type = child.childNodes[0].value.trim();
				}
			}
		});
		rooms.push(room);
	}
	// recurse through tree
	if (node.childNodes) {
		node.childNodes.forEach((child:any) => extractRooms(child));
	}
	return rooms;
}

function extractBuildingString(node): string {
	let building: string;
	if (node.nodeName === 'div' && node.attrs) {
		const isBuildingInfo = node.attrs.some(attr => attr.name === 'id' && attr.value === 'building-info');
		if (isBuildingInfo) {
			node.childNodes.forEach(child => {
				if (child.nodeName === 'h2' && child.childNodes && child.childNodes[0].nodeName === '#text') {
					building = child.childNodes[0].value.trim();
					return building;
				}
			});
		}
	}
	throw new InsightError("Building not found in room HTML file.");
}





