import {
	InsightError
} from "../src/controller/IInsightFacade";
import { clearDisk, getContentFromArchives } from "../test/TestUtil";
import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";
// import IInsightFacade from "../src/controller/InsightFacade";
// import InsightFacade from "../src/controller/InsightFacade";
import {unzipContent} from "../src/utils/JsonHelper";
import * as parse5 from "parse5";
import {
	addBuildingToRooms, addGeolocationData,
	extractBuildingsIndex,
	extractBuildingName,
	extractRooms,
	findTableBodyNode
} from "../src/utils/HTMLHelper";
import {Building, Room} from "../src/models/Room";

use(chaiAsPromised);

describe("HTMLHelper", function () {

	// Declare datasets used in tests. You should add more datasets like this!
	let miniCampus1: string;
	// let miniCampus2: string;
	let indexString: any;
	let fileStrings: string[];
	let fileString1: any;
	let fileString2: any;
	let fileString3: any;

	before(async function () {
		// Just in case there is anything hanging around from a previous run of the test suite
		await clearDisk();

		// const timeout = 20000;
		// this.timeout(timeout);

		// This block runs once and loads the datasets.
		try {
			// This block runs once and loads the datasets.
			miniCampus1 = await getContentFromArchives("miniCampus1.zip");
			// miniCampus2 = await getContentFromArchives("miniCampus2.zip");

			const unzipped = await unzipContent(miniCampus1);
			const indexHTML = unzipped.file("index.htm");
			if (!indexHTML) {
				throw new Error("no index");
			}
			const index = await indexHTML.async("string");
			indexString = parse5.parse(index);

			const directory = unzipped.folder("campus/discover/buildings-and-classrooms/");
			if (!directory) {
				throw new Error("no building files");
			}
			const fileStringsPromises: Promise<string>[] = [];
			directory.forEach((relativePath, file) => {
				if (file.dir) {
					throw new InsightError("file " + relativePath + " is a folder within courses folder");
				}
				fileStringsPromises.push(file.async("string"));
			});
			fileStrings = await Promise.all(fileStringsPromises);
			expect(fileStrings).to.be.an("array");
			const files = 3;
			expect(fileStrings.length).to.deep.equal(files);

			const index0 = 0
			const index1 = 1;
			const index2 = 2;
			fileString1 = parse5.parse(fileStrings[index0]);
			fileString2 = parse5.parse(fileStrings[index1]);
			fileString3 = parse5.parse(fileStrings[index2]);
		} catch (err) {
			throw new Error("Before all hook failed: " + err);
		}
	});

	describe("ExtractBuildingName", function () {
	    beforeEach(async function () {
	        await clearDisk();
	    });

	    afterEach(async function () {
	        await clearDisk();
	    });

	    it("extract building string successfully", async function () {

	        try {
	           const result = extractBuildingName(fileString1);
			   expect(result).to.deep.equal("Biological Sciences");
	        } catch (e) {
	            expect.fail('should not have thrown an error here' + e);
	        }

	    });

	    // it("result too large", async function () {
	    //     try {
	    //         const input1 = 2009
	    //         await getMatchingSections('GT', ['sections_year', input1]);
	    //         expect.fail('should have thrown result too large error')
	    //     } catch (e) {
	    //         expect(e).to.be.instanceOf(ResultTooLargeError);
	    //     }
	    // });

	});

	describe("ExtractBuildingsIndex", function () {

		let tableBodyNode: any;

		beforeEach(async function () {
			tableBodyNode = findTableBodyNode(indexString);
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("extract buildings index successfully", function () {
			try {
				const indexNumBuildings = 74;
				const result = extractBuildingsIndex(tableBodyNode);
				expect(result[0].fullname).to.deep.equal("Acute Care Unit");
				expect(result.length).to.deep.equal(indexNumBuildings);


				// expect(result[0].fullname).to.deep.equal("Biological Sciences");
				// expect(result[1].fullname).to.deep.equal("Chemistry");
				// expect(result[2].fullname).to.deep.equal("Woodward (Instructional Resources Centre-IRC)");
				// expect(result).to.have.members([
				// 	{
				// 		fullname: "Biological Sciences",
				// 		shortname: "BIOL",
				// 		address: "6270 University Boulevard",
				// 		lat: null,
				// 		lon: null,
				// 		href:
				// 	}
				// ])
			} catch (error) {
				expect.fail("ExtractBuildingsIndex failed: " + error);
			}
		});
	});

	describe("ExtractRooms", function () {

		let tableBodyNode: any;

		beforeEach(async function () {
			tableBodyNode = findTableBodyNode(fileString1);
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("extract rooms successfully", async function () {
			try {
				const result = extractRooms(tableBodyNode);
				const first = 0;
				const last = 3;
				const numRooms = 4;
				expect(result[first].number).to.deep.equal("1503");
				expect(result[last].number).to.deep.equal("2519");
				expect(result.length).to.deep.equal(numRooms);
			} catch (e) {
				expect.fail('should not have thrown an error here' + e);
			}
		});
	});

	describe("AddGeolocationData", function () {

		let index: Partial<Building>[];

		beforeEach(async function () {
			const tableBodyNode1 = findTableBodyNode(indexString);
			index = extractBuildingsIndex(tableBodyNode1);

			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("add geolocation data to a building successfully", async function () {
			try {
				let buildingWithGeolocation: Partial<Building>;
				if (index[0].address) {
					buildingWithGeolocation = await addGeolocationData(index[0], index[0].address);
					if (!buildingWithGeolocation.lat || !buildingWithGeolocation.lon) {
						throw new InsightError("No geolocation data added.");
					}
				}
			} catch (e) {
				expect.fail('should not have thrown an error here' + e);
			}
		});
	});

	describe("AddBuildingToRooms", function () {

		let index: Partial<Building>[];
		let rooms1: Partial<Room>[];
		let rooms2: Partial<Room>[];
		let rooms3: Partial<Room>[];
		let buildingString1: string;
		let buildingString2: string;
		let buildingString3: string;
		let buildingPromises: Promise<Partial<Building>>[];
		let indexComplete: Building[];

		beforeEach(async function () {
			const tableBodyNode1 = findTableBodyNode(indexString);
			index = extractBuildingsIndex(tableBodyNode1);

			const tableBodyNode2 = findTableBodyNode(fileString1);
			rooms1 = extractRooms(tableBodyNode2);
			const tableBodyNode3 = findTableBodyNode(fileString2);
			rooms2 = extractRooms(tableBodyNode3);
			const tableBodyNode4 = findTableBodyNode(fileString3);
			rooms3 = extractRooms(tableBodyNode4);

			buildingString1 = extractBuildingName(fileString1);
			buildingString2 = extractBuildingName(fileString2);
			buildingString3 = extractBuildingName(fileString3);

			index.forEach((building: Partial<Building>) => {
				if (building.address) {
					buildingPromises.push(addGeolocationData(building, building.address));
				}
			})
			indexComplete = await Promise.all(buildingPromises) as Building[];

			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("add building to rooms successfully", async function () {
			try {
				const buildingRooms1 = addBuildingToRooms(indexComplete, buildingString1, rooms1);
				const buildingRooms2 = addBuildingToRooms(indexComplete, buildingString2, rooms2);
				const buildingRooms3 = addBuildingToRooms(indexComplete, buildingString3, rooms3);

				const biolRooms = 4;
				const chemRooms = 6;
				const woodRooms = 16;
				expect(buildingRooms1.length).to.deep.equal(biolRooms);
				expect(buildingRooms1[0].building.fullname).to.deep.equal("Biological Sciences");
				expect(buildingRooms2.length).to.deep.equal(chemRooms);
				expect(buildingRooms1[0].building.fullname).to.deep.equal("Chemistry");
				expect(buildingRooms3.length).to.deep.equal(woodRooms);
				expect(buildingRooms1[0].building.fullname).to.deep.equal("Woodward (Instructional Resources Centre-IRC)");
			} catch (e) {
				expect.fail('should not have thrown an error here' + e);
			}
		});
	});
});
