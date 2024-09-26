import {
	IInsightFacade,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	ResultTooLargeError,
	NotFoundError,
} from "../../src/controller/IInsightFacade";
import InsightFacade from "../../src/controller/InsightFacade";
import { clearDisk, getContentFromArchives, loadTestQuery } from "../TestUtil";

import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";

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

	before(async function () {
		// This block runs once and loads the datasets.
		sections = await getContentFromArchives("pair.zip");

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
			await clearDisk();
		});

		it("adding with an empty dataset id", async function () {
			try {
				await facade.addDataset("", sections, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding reject with an id that is whitespace", async function () {
			try {
				await facade.addDataset("    ", sections, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding reject with an id with underscore", async function () {
			try {
				await facade.addDataset(
					"this_id",
					sections,
					InsightDatasetKind.Sections
				);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding invalid string content", async function () {
			const data = "invalid string";
			try {
				await facade.addDataset("id", data, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding invalid kind rooms", async function () {
			try {
				await facade.addDataset("    ", sections, InsightDatasetKind.Rooms);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("same id added twice", async function () {
			const miniData2 = await getContentFromArchives("miniData2.zip");

			try {
				await facade.addDataset("mini", sections, InsightDatasetKind.Sections);
				expect("should have added correctly");
				await facade.addDataset("mini", miniData2, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding empty dataset", async function () {
			try {
				const empty = await getContentFromArchives("emptyTest.zip");
				await facade.addDataset("empty", empty, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding without course folder", async function () {
			try {
				const noCoursesData = await getContentFromArchives("miniData1.zip");
				await facade.addDataset(
					"noCoursesData",
					noCoursesData,
					InsightDatasetKind.Sections
				);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding data with invalid section", async function () {
			try {
				const miniData5 = await getContentFromArchives("miniData5.zip");
				await facade.addDataset(
					"noCoursesData",
					miniData5,
					InsightDatasetKind.Sections
				);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("adding valid large dataset", async function () {
			try {
				await facade.addDataset(
					"noCoursesData",
					sections,
					InsightDatasetKind.Sections
				);
			} catch (err) {
				expect.fail("Should not have thrown an error" + err);
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

		it("removing an empty dataset id", async function () {
			try {
				await facade.removeDataset("");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("removing an id that is whitespace", async function () {
			try {
				await facade.removeDataset("    ");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("removing reject id with underscore", async function () {
			try {
				await facade.removeDataset("this_id");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("removing id that is not in datasets", async function () {
			//const miniData1 = await getContentFromArchives("miniData1.zip");  invalid dataset no courses folder
			//const miniData2 = await getContentFromArchives("miniData2.zip");

			try {
				await facade.addDataset("data", sections, InsightDatasetKind.Sections);
				//expect("should have added sucessfully");
				await facade.removeDataset("badId");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(NotFoundError);
			}
		});

		it("removed dataset still in database", async function () {
			//const miniData1 = await getContentFromArchives("miniData1.zip");  invalid dataset no courses folder
			//const miniData2 = await getContentFromArchives("miniData2.zip");

			try {
				await facade.addDataset("data", sections, InsightDatasetKind.Sections);
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
				await facade.addDataset("data", sections, InsightDatasetKind.Sections);
				const result = await facade.removeDataset("data");
				expect(result).to.equal("data");
			} catch (err) {
				expect.fail("Should have sucessfully added and removed" + err);
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

		it("list from empty database", async function () {
			const result = await facade.listDatasets(); //I don't think I need a try catch here because listDatasets() does not throw any errors

			expect(result).to.deep.equal([]);
		});

		it("list with one entry", async function () {
			try {
				const miniData5 = await getContentFromArchives("miniData3.zip");
				await facade.addDataset(
					"miniData5",
					miniData5,
					InsightDatasetKind.Sections
				);
			} catch (error) {
				expect.fail("addDataset failed" + error);
			}

			const result = await facade.listDatasets(); //I don't think I need a try catch here because listDatasets() does not throw any errors

			expect(result).to.deep.equal([
				{
					id: "miniData5",
					kind: InsightDatasetKind.Sections,
					numRows: 1,
				},
			]);
		});

		it("list with many entries", async function () {
			try {
				//const miniData4 = await getContentFromArchives("miniData4.zip");
				await facade.addDataset(
					"miniData4",
					sections,
					InsightDatasetKind.Sections
				);
			} catch (error) {
				expect.fail("addDataset failed" + error);
			}

			const result = await facade.listDatasets(); //I dont think I need a try catch here because listDatasets() does not throw any errors

			expect(result).to.deep.equal([
				{
					id: "miniData4",
					kind: InsightDatasetKind.Sections,
					numRows: 64612,
				},
			]);
		});

		//List from database with single entry
		//list from datbase with many entries
		//list from database with rooms and sections
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
			const { input, expected, errorExpected } = await loadTestQuery(
				this.test.title
			);
			let result: InsightResult[];

			try {
				result = await facade.performQuery(input);

				if (errorExpected) {
					expect.fail(
						`performQuery resolved when it should have rejected with ${expected}`
					);
				}
				expect(result).to.deep.equal(expected);
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
				expect("performQuery passed threw error when expected"); // TODO: replace with your assertions
				return;
				//Look into resolving promises
			}

			//OTHERWISE I THINK THE TEST SHOULD HAVE PASSED
			//expect results to equal expected from plain UI, expectations should be writen in JSON file
			//expect("PerformQuery passed no error when expected"); // TODO: replace with your assertions
			//return
		}

		before(async function () {
			facade = new InsightFacade();

			// Add the datasets to InsightFacade once.
			// Will *fail* if there is a problem reading ANY dataset.
			const loadDatasetPromises: Promise<string[]>[] = [
				facade.addDataset("sections", sections, InsightDatasetKind.Sections),
			];

			try {
				await Promise.all(loadDatasetPromises);
			} catch (err) {
				throw new Error(
					`In PerformQuery Before hook, dataset(s) failed to be added. \n${err}`
				);
			}
		});

		after(async function () {
			await clearDisk();
		});

		// Examples demonstrating how to test performQuery using the JSON Test Queries.
		// The relative path to the query file must be given in square brackets.
		it("[valid/simple.json] SELECT dept, avg WHERE avg > 97", checkQuery);
		it("[invalid/invalid.json] Query missing WHERE", checkQuery);
		it("[invalid/ReferencingMoreThanOneDatabase.json]", checkQuery);
		it("[invalid/ResultTooLarge.json]", checkQuery);
		it("[valid/valid1.json] WHERE: OR, GT, LT", checkQuery);
		it("[valid/*wildcard.json] *wildcard", checkQuery);
		it("[valid/wildcard*.json] wildcard*", checkQuery);
		it("[valid/*wildcard*.json] *wildcard*", checkQuery);
		it("[invalid/wild*card.json] wild*card", checkQuery);
	});
});
