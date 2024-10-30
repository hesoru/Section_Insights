import * as parse5 from "parse5";
import { InsightError } from "../controller/IInsightFacade";
import { Building, Room } from "../models/Room";
import * as http from "node:http";
import JSZip from "jszip";
import { writeRoomsToDisk } from "./JsonHelper";

export async function parseIndexString(indexString: string): Promise<Building[]> {
	try {
		// parse index HTML into document (node tree)
		const document = parse5.parse(indexString);
		// find buildings table in document and extract buildings from it
		const tableBodyNode = findTableBodyNode(document);

		let buildingsIndex: Partial<Building>[] = [];
		buildingsIndex = extractBuildingsIndex(tableBodyNode, buildingsIndex);
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
		buildingStrings.forEach((file) => {
			// parse rooms HTML into document (node tree)

			const document = parse5.parse(file);
			const buildingString = extractBuildingString(document);
			// find rooms table in document and extract rooms from it
			const tableBodyNode = findTableBodyNode(document);
			if (tableBodyNode) {
				let roomsData: Partial<Room>[] = [];
				roomsData = extractRooms(tableBodyNode, roomsData);
				// match building string to building in index
				const buildingRooms = addBuildingToRooms(buildingsIndex, buildingString, roomsData);
				// push rooms for each building to roomsDataset
				buildingRooms.forEach((room) => {
					roomsDataset.push(room);
				});
			} //else building has no rooms to be added
		});
		return roomsDataset;
	} catch (error) {
		throw new InsightError("failed to parse building strings" + error);
	}
}

async function addGeolocationDataToAll(buildingsIndex: Partial<Building>[]): Promise<Building[]> {
	try {
		// for each building in buildingsIndex:
		const buildingsIndexGeolocation =  await Promise.all(
			buildingsIndex.map(async (building: Partial<Building>) => {
				// encode the address in URL format
				const address = (building as Building).address;
				const addressURL = encodeURIComponent(address);
				try {
					// add geolocation data to building
					return await addGeolocationData(building, addressURL);
				} catch {
					// TODO: what do we do if we don't receive geolocation data? reject the building? YES!
					// return building without geolocation data otherwise
					// return building as Building;
					return null;
				}
			})
		);
		return buildingsIndexGeolocation.filter((building) => (building !== null)) as Building[];
	} catch (error) {
		throw new InsightError("Adding geolocation data failed: " + error);
	}
}

async function addGeolocationData(building: Partial<Building>, addressURL: string): Promise<Building> {
	return new Promise((resolve, reject) => {
		const url = `http://cs310.students.cs.ubc.ca:11316/api/v1/project_team154/${addressURL}`;
		let geoResponse = null;

		// send HTTP GET request
		http
			.get(url, (res) => {
				let data = "";
				// collect response data in chunks
				res.on("data", (chunk) => {
					data += chunk;
				});
				// once request is complete:
				res.on("end", () => {
					try {
						// parse JSON-formatted georesponse into object
						geoResponse = JSON.parse(data);
						// if georesponse does not contain an error, transfer lat/lon into requested building
						if (!geoResponse.error) {
							building.lat = geoResponse.lat;
							building.lon = geoResponse.lon;
						} else {
							reject(new InsightError("Georesponse error returned.")); // reject promise
						}
						resolve(building as Building); // resolve promise
					} catch (error: any) {
						reject(new InsightError(error.message)); // reject promise
					}
				});
			})
			.on("error", (error) => {
				reject(new InsightError(error.message)); // reject promise
			});
	});
}

// match building to roomsData based on given buildingString (from roomsData)
function addBuildingToRooms(buildingsIndex: Building[], buildingString: string, roomsData: Partial<Room>[]): Room[] {
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
		});
		return roomsData as Room[];
	} catch (error) {
		throw new InsightError("Not able to find building in index file that matches building in rooms file: " + error);
	}
}

// recursive function to traverse the document tree and extract table rows
export function findTableBodyNode(node: any): any {
	// verify if node is a table node (return tbody node)
	let tableBody: any = null;
	if (node.nodeName === "table" && node.attrs) {
		// both building and room tables have this specifier - TODO: is it too specific?
		const isTable = node.attrs.some((attr: any) => attr.value.includes("views-table cols-5 table"));
		if (isTable) {
			tableBody = node.childNodes.find((child: any) => child.nodeName === "tbody");
			return tableBody;
		}
	}

	// recurse through tree to find table
	if (node.childNodes) {
		for (const child of node.childNodes) {
			const result = findTableBodyNode(child);
			if (result) {
				return result;
			}
		}
	}
	return tableBody;
	//throw new InsightError("Data table not found in HTML file!");
}

function extractBuildingsIndex(tableBodyNode: any, buildingsIndex: Partial<Building>[]): Partial<Building>[] {
	// extracts full name, shortname, address, and href from index HTML
	// buildings will be partially completed (no geolocation)
	if (tableBodyNode.nodeName === "tr" && tableBodyNode.childNodes) {
		const building: Partial<Building> = {};
		tableBodyNode.childNodes.forEach((child: any) => {
			if (child.nodeName === "td") {
				if (child.attrs?.some((attr: any) => attr.name === "class"
						&& attr.value.includes("views-field-field-building-code"))) {
					building.shortname = child.childNodes[0].value.trim();
				}
				if (child.attrs?.some((attr: any) => attr.name === "class" && attr.value.includes("views-field-title"))) {
					building.fullname = child.childNodes[1].childNodes[0].value.trim(); //problem is here, can not access child of a child, does not exist, changed to index 1 but might need to change to name === 'a'
					building.href = child.childNodes[1].attrs.find((attr: any) => attr.name === "href").value;
				}
				if (child.attrs?.some((attr: any) => attr.name === "class"
						&& attr.value.includes("views-field-field-building-address"))) {
					building.address = child.childNodes[0].value.trim();
				}
			}
		});
		buildingsIndex.push(building);
	}
	// recurse through tree to extract building data
	if (tableBodyNode.childNodes) {
		tableBodyNode.childNodes.forEach((child: any) => extractBuildingsIndex(child, buildingsIndex));
	}
	return buildingsIndex;
}

function extractRooms(node: any, rooms: Partial<Room>[]): Partial<Room>[] {
	// extracts room number, seats, furniture, and type from each room HTML
	// rooms will be partially completed (no building)

	if (node.nodeName === "tr" && node.childNodes) {
		rooms.push(extractChildRooms(node));
	}

	// recurse through tree to extract room data
	if (node.childNodes) {
		node.childNodes.forEach((child: any) => extractRooms(child, rooms));
	}
	return rooms;
}

function extractChildRooms(node: any): Partial<Room> {
	const room: Partial<Room> = {};
	node.childNodes.forEach((child: any) => {
		if (child.nodeName === "td") {
			if (child.attrs.some((attr: any) => attr.name === "class"
				&& attr.value.includes("views-field-field-room-number"))) {
				room.number = child.childNodes[1].childNodes[0].value.trim();
			}
			if (child.attrs.some((attr: any) => attr.name === "class" && attr.value.includes("views-field-field-room-capacity"))) {
				room.seats = child.childNodes[0].value.trim();
			}
			if (
				child.attrs.some(
					(attr: any) => attr.name === "class" && attr.value.includes("views-field-field-room-furniture")
				)
			) {
				room.furniture = child.childNodes[0].value.trim();
			}
			if (
				child.attrs.some((attr: any) => attr.name === "class" && attr.value.includes("views-field-field-room-type"))
			) {
				room.type = child.childNodes[0].value.trim();
			}
		}
	});
	return room;
}

function extractBuildingString(node: any): string {
	// extracts name of building from room HTML
	const buildingString = findBuildingInfo(node);
	if (buildingString) {
		return buildingString;
	}

	throw new InsightError("Building name not found in room HTML file.");
}

function findBuildingInfo(node: any): any {
	let buildingString: string | null = null;
	if (node.nodeName === "div" && node.attrs) {
		const isBuildingInfo = node.attrs.some((attr: any) => attr.name === "id" && attr.value === "building-info");
		if (isBuildingInfo) {
			node.childNodes.forEach((child: any) => {
				// TODO: too hard-coded?
				if (child.nodeName === "h2" && child.childNodes && child.childNodes[0].nodeName === "span") {
					if (child.childNodes[0].childNodes) {
						buildingString = child.childNodes[0].childNodes[0].value.trim();
					}

					return buildingString;
				}
			});
		}
	}
	if (node.childNodes) {
		for (const child of node.childNodes) {
			const result = findBuildingInfo(child);
			if (result) {
				return result;
			}
		}
	}
	return buildingString;
}

export async function addRoomsDataset(
	unzipped: JSZip,
	fileStrings: string[],
	nextAvailableName: number,
	id: string
): Promise<Set<Room>> {
	const added = new Set<Room>();
	const indexHTML = unzipped.file("index.htm");
	if (!indexHTML) {
		throw new InsightError("index.htm not found in dataset");
	}
	const indexString = await indexHTML.async("string");
	const buildings = await parseIndexString(indexString);
	const roomsDataset = parseBuildingStrings(fileStrings, buildings);
	roomsDataset.forEach((room) => added.add(room));
	await writeRoomsToDisk(roomsDataset, nextAvailableName, id);
	return added;
}
