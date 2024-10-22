import parse5, {DefaultTreeAdapterMap} from 'parse5';
import fs from 'fs';
import JSZip from "jszip";
import {InsightDataset, InsightError} from "../controller/IInsightFacade";
import {unzipContent} from "./JsonHelper";
import {Building, Room} from "../models/Section";
import { Document } from 'parse5/dist/tree-adapters/default';



export function parseIndexString(indexString: string): Building[] {
	const document = parse5.parse(indexString);
	const tableBodyNode = findTableBodyNode(document);
	return extractBuildingsIndex(tableBodyNode);
}

export function parseBuildingStrings(buildingStrings: string[], buildingsIndex: Building[]): Room[] {
	const roomsDataset: Room[] = [];
	buildingStrings.forEach(file => {
		const document = parse5.parse(file);
		const buildingString = extractBuildingStringForRooms();
		// getLocationData;
		const tableBodyNode = findTableBodyNode(document);
		const buildingRooms = extractRooms(tableBodyNode);
		const buildingData = addBuildingToRooms(buildingsIndex, buildingString, buildingRooms);
		roomsDataset.push(buildingData);
	});
	return roomsDataset;
}

function addBuildingToRooms(buildingsIndex: Building[], buildingString: string, buildingRooms: Room[]): Room[] {
	let foundBuilding = {} as Building;
	// every() behaves like foreach(), except stops iterating when receiving a false value
	buildingsIndex.every((building) => {
		if (building.fullname === buildingString) {
			foundBuilding = building;
			if (foundBuilding) {
				return false;
			}
		}
		return true;
	})
	if (!foundBuilding) {
		// invalid dataset!
		return [];
	}
	buildingRooms.forEach((room) => {
		room.building = foundBuilding;
	})
	return buildingRooms;
}

// recursive function to traverse the document tree and extract table rows
export function findTableBodyNode(node: { nodeName: string; attrs: any[]; childNodes: any; }): { nodeName: string; attrs: any[]; childNodes: any; } {
	// verify if node is a table node (return tbody node)
	if (node.nodeName === 'table' && node.attrs) {
		const isTableNode = node.attrs.some(attr => attr.value.includes('views-table cols-5 table'));
		if (isTableNode) {
			return node.childNodes.find(child => child.nodeName === 'tbody');
		}
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

function extractBuildingsIndex(node): Building[] {
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

function extractRooms(node): Room[] {
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

function extractBuildingStringForRooms(node): string {
	let building: string;
	if (node.nodeName === 'div' && node.attrs) {
		const isBuildingInfo = node.attrs.some(attr => attr.name === 'id' && attr.value === 'building-info');
		if (isBuildingInfo) {
			node.childNodes.forEach(child => {
				if (child.nodeName === 'h2' && child.childNodes && child.childNodes[0].nodeName === '#text') {
					building = child.childNodes[0].value.trim();
					if (building) {
						return building;
					}
				}
			});
		}
	}
	throw new InsightError("Building not found for room HTML file.");
}





