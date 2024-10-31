import {
	IInsightFacade,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError,
} from "../../src/controller/IInsightFacade";
import InsightFacade from "../../src/controller/InsightFacade";
import { clearDisk, getContentFromArchives, loadTestQuery } from "../TestUtil";

import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";
import fs, { readdir } from "fs-extra";
import path from "node:path";

use(chaiAsPromised);

export interface ITestQuery {
	title?: string;
	input: unknown;
	errorExpected: boolean;
	expected: any;
}

describe("InsightFacade", function () {
	let facade: IInsightFacade;

	// Declare datasets used in tests. You should add more datasets like this!
	let sections: string;
	let miniAddDataset: string;
	let miniCampus1: string;
	let miniCampus2: string;
	let rooms: string;

	before(async function () {
		// This block runs once and loads the datasets.
		sections = await getContentFromArchives("pair.zip");
		miniAddDataset = await getContentFromArchives("miniAddData.zip");
		miniCampus1 = await getContentFromArchives("miniCampus1.zip");
		miniCampus2 = await getContentFromArchives("miniCampus2.zip");
		rooms = await getContentFromArchives("campus.zip");

		// Just in case there is anything hanging around from a previous run of the test suite
		await clearDisk();
	});

	describe("AddDataset", function () {
		beforeEach(async function () {
			// This section resets the insightFacade instance
			// This runs before each test
			await clearDisk();
			facade = new InsightFacade();
		});

		afterEach(async function () {
			// This section resets the data directory (removing any cached data)
			// This runs after each test, which should make each test independent of the previous one
			//await clearDisk();
		});

		it("should reject adding an empty dataset id", async function () {
			try {
				await facade.addDataset("", miniAddDataset, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding an id that is only whitespace", async function () {
			try {
				await facade.addDataset(" ", miniAddDataset, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding an id with underscore", async function () {
			try {
				await facade.addDataset("this_id", miniAddDataset, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding with wrong type of dataset (not base64 string)", async function () {
			const data = "invalid string";
			try {
				await facade.addDataset("id", data, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		// TODO: how to test InsightDatasetKind that isn't sections or rooms?
		// it("should reject adding with invalid InsightDatasetKind", async function () {
		// 	try {
		// 		await facade.addDataset("mini", miniAddDataset, InsightDatasetKind.Sections);
		// 		expect.fail("Should have thrown above.");
		// 	} catch (err) {
		// 		expect(err).to.be.instanceOf(InsightError);
		// 	}
		// });

		it("should reject adding same id twice", async function () {
			const miniData5 = await getContentFromArchives("miniData5.zip");
			try {
				await facade.addDataset("mini", miniData5, InsightDatasetKind.Sections);
				expect("should have added correctly");
				await facade.addDataset("mini", miniData5, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding with dataset that is an empty .zip file", async function () {
			try {
				const empty = await getContentFromArchives("emptyTest.zip");
				await facade.addDataset("empty", empty, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding without courses folder", async function () {
			try {
				const noCoursesData = await getContentFromArchives("miniData1.zip");
				await facade.addDataset("noCoursesData", noCoursesData, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding dataset with invalid sections", async function () {
			try {
				const miniData6 = await getContentFromArchives("miniData6.zip");
				await facade.addDataset("noCoursesData", miniData6, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject with courses in wrong format (not JSON-formatted)", async function () {
			const notJsonFormat = await getContentFromArchives("not_JSON_format.zip");
			try {
				await facade.addDataset("sections", notJsonFormat, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("checking persistence add 2 sections", async function () {
			try {
				const result = await facade.addDataset("sections", miniAddDataset, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["sections"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "sections",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
				]);
				const newFacade = new InsightFacade();
				const miniData5 = await getContentFromArchives("miniData5.zip");
				const result1 = await newFacade.addDataset("mini5", miniData5, InsightDatasetKind.Sections);
				expect(result1).to.deep.equal(["sections", "mini5"]);

				const datasets = await newFacade.listDatasets();
				expect(datasets).to.have.deep.members([
					{
						id: "sections",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
					{
						id: "mini5",
						kind: InsightDatasetKind.Sections,
						numRows: 6,
					},
				]);
				// read file from disk
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
			}
		});

		it("checking persistence add 2 rooms datasets", async function () {
			try {
				const result = await facade.addDataset("miniCampus1", miniCampus1, InsightDatasetKind.Rooms);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["miniCampus1"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "miniCampus1",
						kind: InsightDatasetKind.Rooms,
						numRows: 26,
					},
				]);
				const newFacade = new InsightFacade();
				const result1 = await newFacade.addDataset("miniCampus2", miniCampus2, InsightDatasetKind.Rooms);
				expect(result1).to.deep.equal(["miniCampus1", "miniCampus2"]);

				const datasets = await newFacade.listDatasets();
				expect(datasets).to.have.deep.members([
					{
						id: "miniCampus1",
						kind: InsightDatasetKind.Rooms,
						numRows: 26,
					},
					{
						id: "miniCampus2",
						kind: InsightDatasetKind.Rooms,
						numRows: 6,
					},
				]);
				// read file from disk
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
			}
		});

		it("should successfully add valid large Sections dataset, and create file on disk", async function () {
			try {
				const result = await facade.addDataset("miniAdd", miniAddDataset, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["miniAdd"]);
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
			}
		});

		it("checking persistence for add followed by a query", async function () {
			//this.timeout(10000);
			try {
				const result = await facade.addDataset("sections", sections, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["sections"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "sections",
						kind: InsightDatasetKind.Sections,
						numRows: 64612,
					},
				]);
				const newFacade = new InsightFacade();
				const miniData5 = await getContentFromArchives("miniData5.zip");
				const result1 = await newFacade.addDataset("mini5", miniData5, InsightDatasetKind.Sections);
				expect(result1).to.deep.equal(["sections", "mini5"]);
				const datasets = await newFacade.listDatasets();
				expect(datasets).to.have.deep.members([
					{
						id: "sections",
						kind: InsightDatasetKind.Sections,
						numRows: 64612,
					},
					{
						id: "mini5",
						kind: InsightDatasetKind.Sections,
						numRows: 6,
					},
				]);

				const query = {
					WHERE: {
						GT: {
							sections_avg: 97,
						},
					},
					OPTIONS: {
						COLUMNS: ["maxAvg", "sections_dept"],
						ORDER: "maxAvg",
					},
					TRANSFORMATIONS: {
						GROUP: ["sections_dept"],
						APPLY: [
							{
								maxAvg: {
									MAX: "sections_avg",
								},
							},
						],
					},
				};

				await newFacade.performQuery(query);
				// read file from disk
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
			}
		});

		it("should successfully add valid Rooms dataset, and create file on disk", async function () {
			try {
				const result = await facade.addDataset("miniCampus", miniCampus1, InsightDatasetKind.Rooms);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["miniCampus"]);

				const dataset = await facade.listDatasets();
				expect(dataset).to.have.deep.members([
					{
						id: "miniCampus",
						kind: InsightDatasetKind.Rooms,
						numRows: 26,
					},
				]);
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
			}
		});

		it("should successfully add valid large Rooms dataset, and create file on disk", async function () {
			//this.timeout(10000);
			try {
				const result = await facade.addDataset("rooms", rooms, InsightDatasetKind.Rooms);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["rooms"]);

				const dataset = await facade.listDatasets();
				expect(dataset).to.have.deep.members([
					{
						id: "rooms",
						kind: InsightDatasetKind.Rooms,
						numRows: 364,
					},
				]);
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
			}
		});

		it("checking persistence add same sections dataset twice", async function () {
			try {
				const result = await facade.addDataset("mini", miniAddDataset, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["mini"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "mini",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
				]);
				const newFacade = new InsightFacade();
				await newFacade.addDataset("mini", miniAddDataset, InsightDatasetKind.Sections);
				expect.fail("should not have been able to add dataset with the same id");
			} catch (e) {
				expect(e).to.be.instanceOf(InsightError);
			}
		});
	});

	describe("RemoveDataset", function () {
		beforeEach(async function () {
			// This section resets the insightFacade instance
			// This runs before each test
			await clearDisk();
			facade = new InsightFacade();
		});

		afterEach(async function () {
			// This section resets the data directory (removing any cached data)
			// This runs after each test, which should make each test independent of the previous one
			await clearDisk();
		});

		it("should reject removing an empty dataset id", async function () {
			try {
				await facade.removeDataset("");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject removing an id that is only whitespace", async function () {
			try {
				await facade.removeDataset(" ");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject removing reject id with underscore", async function () {
			try {
				await facade.removeDataset("this_id");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject removing id that is not in datasets", async function () {
			try {
				await facade.addDataset("data", miniAddDataset, InsightDatasetKind.Sections);
				await facade.removeDataset("badId");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(NotFoundError);
			}
		});

		it("removed dataset still in database", async function () {
			try {
				await facade.addDataset("data", miniAddDataset, InsightDatasetKind.Sections);
				await facade.removeDataset("data");
			} catch (err) {
				expect.fail("Should have sucessfully added and removed" + err);
			}

			const results = await facade.listDatasets();
			const contains: boolean = results.some((result) => result.id === "data");

			if (contains) {
				expect.fail("Should have removed the id");
			} else {
				expect("correctly removed dataset");
			}
		});

		it("promise does not resolve to correct string", async function () {
			//const miniData1 = await getContentFromArchives("miniData1.zip");  invalid dataset no courses folder
			//const miniData2 = await getContentFromArchives("miniData2.zip");
			try {
				await facade.addDataset("data", miniAddDataset, InsightDatasetKind.Sections);
				const result = await facade.removeDataset("data");
				expect(result).to.equal("data");
			} catch (err) {
				expect.fail("Should have successfully added and removed" + err);
			}
		});

		it("checking persistence remove", async function () {
			try {
				const result = await facade.addDataset("mini", miniAddDataset, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["mini"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "mini",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
				]);

				const newFacade = new InsightFacade();
				const result1 = await newFacade.removeDataset("mini");
				expect(result1).to.equal("mini");

				const datasets = await newFacade.listDatasets();
				expect(datasets).to.deep.equal([]);
			} catch (err) {
				expect.fail("should not have thrown err" + err);
			}
		});

		it("checking persistence remove and erase", async function () {
			try {
				const result = await facade.addDataset("mini", miniAddDataset, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["mini"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "mini",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
				]);

				const newFacade = new InsightFacade();
				const result1 = await newFacade.removeDataset("mini");
				expect(result1).to.equal("mini");

				const datasets = await newFacade.listDatasets();
				expect(datasets).to.deep.equal([]);
				const fileNames = await readdir("./data");
				const promises = [];
				for (const file of fileNames) {
					const filePath = path.resolve("./data", file);
					promises.push(fs.readJson(filePath));
				}

				try {
					const files = await Promise.all(promises);
					const ids = [];
					for (const item of files) {
						const id = item.datasetID;
						ids.push(id);
					}
					expect(ids).to.not.include("mini");
				} catch {
					expect.fail("should not have thrown an exception");
				}
			} catch (err) {
				expect.fail("should not have thrown err" + err);
			}
		});
	});

	describe("ListDatasets", function () {
		beforeEach(async function () {
			// This section resets the insightFacade instance
			// This runs before each test
			await clearDisk();
			facade = new InsightFacade();
		});

		afterEach(async function () {
			// This section resets the data directory (removing any cached data)
			// This runs after each test, which should make each test independent of the previous one
			await clearDisk();
		});

		it("successfully lists datasets, with no existing datasets", async function () {
			const result = await facade.listDatasets(); // I don't think I need a try catch here because listDatasets() does not throw any errors
			const arrayLength = 0;

			expect(result).to.deep.equal([]);
			expect(result.length).to.equal(arrayLength);
		});

		it("successfully lists datasets, with one existing dataset", async function () {
			try {
				const miniData5 = await getContentFromArchives("miniData5.zip");
				await facade.addDataset("miniData5", miniData5, InsightDatasetKind.Sections);
			} catch (error) {
				expect.fail("addDataset failed" + error);
			}

			const result = await facade.listDatasets();
			expect(result).to.deep.equal([
				{
					id: "miniData5",
					kind: InsightDatasetKind.Sections,
					numRows: 6,
				},
			]);
			const arrayLength = 1;
			expect(result.length).to.equal(arrayLength);
		});

		it("successfully lists datasets, with many existing datasets", async function () {
			const timeout = 10000;
			this.timeout(timeout);
			try {
				const miniData5 = await getContentFromArchives("miniData5.zip");
				await facade.addDataset("miniData4", miniAddDataset, InsightDatasetKind.Sections);
				await facade.addDataset("miniData5", miniData5, InsightDatasetKind.Sections);
			} catch (error) {
				expect.fail("addDataset failed" + error);
			}

			const result = await facade.listDatasets();
			expect(result).to.deep.equal([
				{
					id: "miniData4",
					kind: InsightDatasetKind.Sections,
					numRows: 108,
				},
				{
					id: "miniData5",
					kind: InsightDatasetKind.Sections,
					numRows: 6,
				},
			]);
			const arrayLength = 2;
			expect(result.length).to.equal(arrayLength);
		});

		it("checking persistence list", async function () {
			try {
				const result = await facade.addDataset("sections", miniAddDataset, InsightDatasetKind.Sections);
				expect(result).to.be.an("array");
				expect(result).to.deep.equal(["sections"]);
				const dataset = await facade.listDatasets();
				expect(dataset).to.deep.equal([
					{
						id: "sections",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
				]);

				const newFacade = new InsightFacade();
				const result1 = await newFacade.listDatasets();
				expect(result1).to.deep.equal([
					{
						id: "sections",
						kind: InsightDatasetKind.Sections,
						numRows: 108,
					},
				]);
			} catch (err) {
				expect.fail("should not have thrown err" + err);
			}
		});
	});

	describe("PerformQuery", function () {
		/**
		 * Loads the TestQuery specified in the test name and asserts the behaviour of performQuery.
		 *
		 * Note: the 'this' parameter is automatically set by Mocha and contains information about the test.
		 */
		async function checkQuery(this: Mocha.Context): Promise<void> {
			if (!this.test) {
				throw new Error(
					"Invalid call to checkQuery." +
						"Usage: 'checkQuery' must be passed as the second parameter of Mocha's it(..) function." +
						"Do not invoke the function directly."
				);
			}
			// Destructuring assignment to reduce property accesses
			const { input, expected, errorExpected } = await loadTestQuery(this.test.title);
			let result: InsightResult[];

			try {
				result = await facade.performQuery(input);

				if (errorExpected) {
					expect.fail(`performQuery resolved when it should have rejected with ${expected}`);
				}
				const expectedLength = expected.length;
				//console.log(expected)
				//console.log(result)
				expect(result.length).to.equal(expectedLength);
				expect(result).to.have.deep.members(expected);
				//try {
				// for (const member of result) {
				// 	let match = 0;
				// 	for (const item of expected) {
				// 		if (JSON.stringify(item) === JSON.stringify(member)) {
				// 			match = 1;
				// 			break;
				// 		}
				// 	}
				// 	if (match === 0) {
				// 		console.log(member);
				// 	}
				// }
				// console.log("expected now....");
				// for (const member of expected) {
				// 	let match = 0;
				// 	for (const item of result) {
				// 		if (JSON.stringify(item) === JSON.stringify(member)) {
				// 			match = 1;
				// 			break;
				// 		}
				// 	}
				// 	if (match === 0) {
				// 		console.log(member);
				// 	}
				// }
				//} catch (er) {
				//console.log(er);
				//}
				const validInput = input as {
					OPTIONS: {
						ORDER?:
							| string
							| {
									dir: "UP" | "DOWN";
									keys: string[];
							  };
					};
				};
				if (validInput.OPTIONS.ORDER) {
					if (typeof validInput.OPTIONS.ORDER === "string") {
						const field = validInput.OPTIONS.ORDER;
						for (let i = 1; i < result.length; i++) {
							if (result[i][field] < result[i - 1][field]) {
								expect.fail("not in correct order");
							}
						}
					}
				}
				if (typeof validInput.OPTIONS.ORDER === "object") {
					const comp: (a: any, b: any) => boolean =
						validInput.OPTIONS.ORDER.dir === "UP"
							? (a: any, b: any): boolean => a < b
							: (a: any, b: any): boolean => a > b;

					for (let i = 1; i < result.length; i++) {
						for (const key of validInput.OPTIONS.ORDER.keys) {
							if (comp(result[i][key], result[i - 1][key])) {
								expect.fail("results are not in the correct order");
							} else if (result[i][key] === result[i - 1][key]) {
								//do nothing
							} else {
								break;
							}
						}
					}
				}

				return;
			} catch (err) {
				if (!errorExpected) {
					expect.fail(`performQuery threw unexpected error: ${err}`);
				}
				//OTHERWISE I THINK THE TEST SHOULD PASS BECAUSE YOU EXPECTED A FAIL
				//result holds returned values which should match expected from json file.
				//expected specified in const {input, expected, errorExpected} during destructuring
				//should fail if result does not equal expected.
				//InsightResult = Record<string, string | number>;

				//Specify types of errors...
				if (expected === "ResultTooLargeError") {
					expect(err).to.be.instanceOf(ResultTooLargeError);
				}

				if (expected === "InsightError") {
					expect(err).to.be.instanceOf(InsightError);
				}
				expect("performQuery passed threw error when expected");
				return;
				//Look into resolving promises
			}

			//OTHERWISE I THINK THE TEST SHOULD HAVE PASSED
			//expect results to equal expected from plain UI, expectations should be writen in JSON file
			//expect("PerformQuery passed no error when expected");
			//return
		}

		before(async function () {
			//this.timeout(10000);
			await clearDisk();
			facade = new InsightFacade();
			// Add the datasets to InsightFacade once.
			// Will *fail* if there is a problem reading ANY dataset.
			try {
				await facade.addDataset("rooms", rooms, InsightDatasetKind.Rooms);
				await facade.addDataset("sections", sections, InsightDatasetKind.Sections);
			} catch (err) {
				throw new Error(`In PerformQuery Before hook, dataset(s) failed to be added. \n${err}`);
			}
		});

		after(async function () {
			//await clearDisk();
		});

		// Examples demonstrating how to test performQuery using the JSON Test Queries.
		// The relative path to the query file must be given in square brackets.

		it("[valid/simple.json] SELECT dept, avg WHERE avg > 97", checkQuery);
		it("[invalid/invalid.json] Query missing WHERE", checkQuery);
		it("[invalid/queryingMultipleDatasets.json]", checkQuery);
		it("[invalid/ResultTooLarge.json]", checkQuery);
		it("[valid/valid1.json] WHERE: OR, GT, LT", checkQuery);
		it("[valid/wildcardC.json] *wildcard", checkQuery);
		it("[valid/wildcardC.json] wildcard*", checkQuery);
		it("[valid/wildcard2astrix.json] wildcard2astrix.json", checkQuery);
		it("[invalid/wildCcard.json] wild*card", checkQuery);
		it("[valid/complexValidQuery.json]", checkQuery);
		it("[invalid/columnsMissing.json]", checkQuery);
		it("[invalid/invalidSField.json]", checkQuery);
		it("[invalid/invalidMField.json]", checkQuery);
		it("[invalid/wrongSyntaxNOT.json]", checkQuery);
		it("[invalid/noFilter.json]", checkQuery);
		it("[invalid/invalid.json]", checkQuery);
		it("[invalid/orderKeyMissing.json]", checkQuery);
		it("[invalid/idDoesNotExist.json]", checkQuery);
		it("[valid/negativeAverage(Valid).json]", checkQuery);
		it("[valid/filterNOT.json]", checkQuery); //works but timesout at 2.47
		it("[invalid/notObject.json]", checkQuery);
		it("[invalid/badWHERE.json]", checkQuery);
		it("[invalid/badOptions.json]", checkQuery);
		it("[invalid/badOR.json]", checkQuery);
		it("[invalid/emptyOptions.json]", checkQuery);
		it("[valid/futureYear(Valid).json]", checkQuery);
		it("[invalid/invalidFilterKeyXOR.json]", checkQuery);
		it("[valid/sortOnString.json]", checkQuery);
		it("[invalid/badID.json]", checkQuery);
		it("[valid/AND.GT.IS.json]", checkQuery);
		it("[valid/nestedLogicals.json]", checkQuery);
		it("[valid/wildcardNOT.json]", checkQuery);
		it("[valid/2Wildcards.json]", checkQuery);
		it("[valid/allFilters.json]", checkQuery);
		it("[valid/year1900(Valid).json]", checkQuery);
		//it("[valid/nestedNot.json]", checkQuery);

		//Queries with Transformations
		it("[validTrans/simpleGDeptAMax.json]", checkQuery);
		it("[validTrans/simpleGDeptASum.json]", checkQuery);
		it("[validTrans/simpleGPassAavg.json]", checkQuery);
		it("[validTrans/twoGroupFields.json]", checkQuery);
		it("[validTrans/manyGroupFields.json]", checkQuery);
		it("[invalidTrans/deptColNOTGroup.json]", checkQuery);
		it("[invalidTrans/emptyGroupArray.json]", checkQuery);
		it("[invalidTrans/noGroup.json]", checkQuery);
		it("[invalidTrans/nonExistantGroupKey.json]", checkQuery);
		it("[validTrans/multipleApplyKeys.json]", checkQuery);
		it("[validTrans/allApplyKeys.json]", checkQuery);
		it("[validTrans/duplicateApplyKeys.json]", checkQuery);
		it("[invalidTrans/duplicateApplyAvg.json]", checkQuery);
		it("[validTrans/countOnString.json]", checkQuery);
		it("[invalidTrans/maxOnString.json]", checkQuery);
		it("[validTrans/noContent.json]", checkQuery);

		//Queries with Sorting
		it("[validSort/sortUp.json]", checkQuery);
		it("[validSort/sortDown.json]", checkQuery);
		it("[validSort/multipleKeysUp.json]", checkQuery);
		it("[validSort/multipleKeysDown.json]", checkQuery);
		it("[invalidSort/badDir.json]", checkQuery);
		it("[invalidSort/badKey.json]", checkQuery);
		it("[invalidSort/notInColumns.json]", checkQuery);
		it("[invalidSort/emptyKeys.json]", checkQuery);
		it("[validSort/duplicateKey.json]", checkQuery);

		//Rooms Queries
		it("[validRooms/complex.json]", checkQuery);
		it("[validRooms/AND.IS.GT.json]", checkQuery);
		it("[validRooms/NOTfilter.json]", checkQuery);
		it("[validRooms/allFilters.json]", checkQuery);
		it("[validRooms/3apply.json]", checkQuery);
		it("[validRooms/max1.json]", checkQuery);
		it("[validRooms/max2.json]", checkQuery);
		it("[invalidRooms/avgFullname.json]", checkQuery);
		it("[invalidRooms/avgHref.json]", checkQuery);
		it("[invalidRooms/maxFurniture.json]", checkQuery);
		it("[invalidRooms/minAddress.json]", checkQuery);
		it("[invalidRooms/minType.json]", checkQuery);
	});
});
