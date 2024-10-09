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

		it("should reject adding an empty dataset id", async function () {
			try {
				await facade.addDataset("", sections, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding an id that is only whitespace", async function () {
			try {
				await facade.addDataset(" ", sections, InsightDatasetKind.Sections);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding an id with underscore", async function () {
			try {
				await facade.addDataset("this_id", sections, InsightDatasetKind.Sections);
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

		it("should reject adding with invalid InsightDatasetKind (ie. not Sections)", async function () {
			try {
				await facade.addDataset("    ", sections, InsightDatasetKind.Rooms);
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should reject adding same id twice", async function () {
			const miniData5 = await getContentFromArchives("miniData5.zip"); // TODO: where is this??
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
				const miniData6 = await getContentFromArchives("miniData6.zip"); // TODO: where is this??
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

		// it("should successfully add valid large dataset, and create file on disk", async function () {
		// 	try {
		// 		const result = await facade.addDataset("sections", sections, InsightDatasetKind.Sections);
		// 		expect(result).to.be.an("array");
		// 		expect(result).to.deep.equal(['sections']);
		// 		// read file from disk
		// 	} catch (err) {
		// 		expect.fail("Should not have thrown an error" + err);
		// 	}
		// });
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
				await facade.addDataset("data", sections, InsightDatasetKind.Sections);
				await facade.removeDataset("badId");
				expect.fail("Should have thrown above.");
			} catch (err) {
				expect(err).to.be.instanceOf(NotFoundError);
			}
		});

		//NOT ALLOWED TO ACCESS FILE PATH OUTSIDE OF TEST FOLDER
		// it("should successfully remove a dataset, and delete it from disk", async function () {
		// 	try {
		// 		await facade.addDataset("mcgill", sections, InsightDatasetKind.Sections);
		// 		const removeResult = await facade.removeDataset("mcgill");
		// 		expect(removeResult).to.equal("mcgill");
		// 	} catch (err) {
		// 		expect.fail("Should have successfully added and removed" + err);
		// 	}
		//
		// 	try {
		// 		// attempt to read deleted file from disk
		// 		const datasetPath = path.resolve(__dirname, "../data", "mcgill");
		// 		await fs.readFile(datasetPath, "utf8");
		// 		expect.fail("Should not be able to read deleted file from disk.");
		// 	} catch {
		// 		// use listDataset() to check that dataset deleted
		// 		const results = await facade.listDatasets();
		// 		const contains: boolean = results.some((result) => result.id === "mcgill");
		// 		if (contains) {
		// 			expect.fail("Should have removed the id");
		// 		}
		// 	}
		// });

		it("promise does not resolve to correct string", async function () {
			//const miniData1 = await getContentFromArchives("miniData1.zip");  invalid dataset no courses folder
			//const miniData2 = await getContentFromArchives("miniData2.zip");
			try {
				await facade.addDataset("data", sections, InsightDatasetKind.Sections);
				const result = await facade.removeDataset("data");
				expect(result).to.equal("data");
			} catch (err) {
				expect.fail("Should have successfully added and removed" + err);
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

		// it("successfully lists datasets, with one existing dataset", async function () {
		// 	try {
		// 		const miniData5 = await getContentFromArchives("miniData5.zip");
		// 		await facade.addDataset("miniData5", miniData5, InsightDatasetKind.Sections);
		// 	} catch (error) {
		// 		expect.fail("addDataset failed" + error);
		// 	}
		//
		// 	const result = await facade.listDatasets();
		// 	expect(result).to.deep.equal([
		// 		{
		// 			id: "miniData5",
		// 			kind: InsightDatasetKind.Sections,
		// 			numRows: 6,
		// 		},
		// 	]);
		// 	const arrayLength = 1;
		// 	expect(result.length).to.equal(arrayLength);
		// });
		//
		// it("successfully lists datasets, with many existing datasets", async function () {
		// 	const timeout = 10000;
		// 	this.timeout(timeout);
		// 	try {
		// 		const miniData5 = await getContentFromArchives("miniData5.zip");
		// 		await facade.addDataset("miniData4", sections, InsightDatasetKind.Sections);
		// 		await facade.addDataset("miniData5", miniData5, InsightDatasetKind.Sections);
		// 	} catch (error) {
		// 		expect.fail("addDataset failed" + error);
		// 	}
		//
		// 	const result = await facade.listDatasets();
		// 	expect(result).to.deep.equal([
		// 		{
		// 			id: "miniData4",
		// 			kind: InsightDatasetKind.Sections,
		// 			numRows: 64612,
		// 		},
		// 		{
		// 			id: "miniData5",
		// 			kind: InsightDatasetKind.Sections,
		// 			numRows: 6,
		// 		},
		// 	]);
		// 	const arrayLength = 2;
		// 	expect(result.length).to.equal(arrayLength);
		// });
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
				expect(input).to.be.an("object");

				if (errorExpected) {
					expect.fail(`performQuery resolved when it should have rejected with ${expected}`);
				}
				expect(result).to.deep.equal(expected);
				return;
			} catch (err) {
				if (!errorExpected) {
					expect.fail(`performQuery threw unexpected error: ${err}`);
				}
				//Specify types of errors...
				if (expected === "ResultTooLargeError") {
					expect(err).to.be.instanceOf(ResultTooLargeError);
				}
				if (expected === "InsightError") {
					expect(err).to.be.instanceOf(InsightError);
				}
				expect("performQuery passed threw error when expected");
				return;
			}
		}

		before(async function () {
			facade = new InsightFacade();

			// Add the datasets to InsightFacade once.
			// Will *fail* if there is a problem reading ANY dataset.
			const loadDatasetPromises: Promise<string[]>[] = [
				facade.addDataset("sections", sections, InsightDatasetKind.Sections),
				facade.addDataset("mcgill", sections, InsightDatasetKind.Sections),
			];

			try {
				await Promise.all(loadDatasetPromises);
			} catch (err) {
				throw new Error(`In PerformQuery Before hook, dataset(s) failed to be added. \n${err}`);
			}
		});

		after(async function () {
			await clearDisk();
		});

		// Examples demonstrating how to test performQuery using the JSON Test Queries.
		// The relative path to the query file must be given in square brackets.

		// invalid inputs //

		// general
		//it("[invalid/resultTooLarge.json] Result Too Large", checkQuery);
		it("[invalid/noFilter.json] No Filter (ResultTooBig)", checkQuery); //good
		it("[invalid/queryingMultipleDatasets.json] Querying Multiple Datasets", checkQuery);

		// improper IDString
		it("[invalid/idDoesNotExist.json] ID Does Not Exist", checkQuery); //good
		it("[invalid/idStringEmpty.json] IDString Empty (Invalid)", checkQuery); //good, invalid Skey which is true

		// improper InputString
		it("[invalid/wildcard(C_S).json] Wildcard (C*S)", checkQuery); //good

		// improper EBNF formatting
		it("[invalid/orderKeyMissing.json] ORDER's key not found in COLUMN's KEY_LIST array", checkQuery); //good
		//it("[invalid/invalidEmptyColumns.json] Query missing WHERE", checkQuery);
		it("[invalid/mkeyUsedForIS.json] mkey used for IS", checkQuery); //good
		//it("[invalid/skeyUsedForMComparator.json] skey used for mcomparator", checkQuery);
		it("[invalid/invalidSField.json] Invalid SField", checkQuery); //good
		it("[invalid/invalidMField.json] Invalid MField", checkQuery); //good
		it("[invalid/columnsMissing.json] COLUMNS Missing", checkQuery); //good
		it("[invalid/invalidFilterKeyXOR.json] Invalid Filter Key: XOR", checkQuery); //good
		it("[invalid/wrongSyntaxNOT.json] Wrong Syntax: NOT", checkQuery); //good

		// valid inputs //

		// general
		//it("[valid/valid1.json] WHERE: OR, GT, LT", checkQuery);
		it("[valid/simple.json] SELECT dept, avg WHERE avg > 97", checkQuery); //close, misorder of rows with same avg ordering
		it("[valid/complexValidQuery.json] Complex Valid Query", checkQuery); //same misordering
		it("[valid/notFound.json] Result Not Found", checkQuery); //good

		// proper InputString
		it("[valid/wildcard(_C).json] Wildcard (*C)", checkQuery); //same misordering right number of rows
		it("[valid/wildcard(C_).json] Wildcard (C*)", checkQuery); //same thing
		it("[valid/_wildcard_.json] Wildcard (*C*)", checkQuery);
		it("[valid/inputStringEmpty(Valid).json] InputString Empty (Valid)", checkQuery);

		// proper number
		it("[valid/year1900(Valid).json] Year 1900", checkQuery);
		it("[valid/negativeAverage(Valid).json] Negative Average (Valid)", checkQuery);
		it("[valid/futureYear(Valid).json] Future Year (Valid)", checkQuery);

		// proper EBNF formatting
		it("[valid/orderMissing(Valid).json] ORDER Missing (Valid)", checkQuery);
		it("[valid/rightSyntaxNOT(Valid).json] Right Syntax: NOT", checkQuery);
	});
});
