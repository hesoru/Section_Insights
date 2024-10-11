import {
	InsightDatasetKind,
	InsightError,
	InsightResult
} from "../src/controller/IInsightFacade";
import { clearDisk, getContentFromArchives, loadTestQuery } from "../test/TestUtil";

import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";
import {
	extractDatasetId,
	getAllSections,
	handleFilter, handleLogicComparison, handleMComparison,
	handleSComparison,
	loadDatasets,
	sortResults
} from "../src/utils/QueryHelper";
import IInsightFacade from "../src/controller/InsightFacade";
import InsightFacade from "../src/controller/InsightFacade";
import {Query} from "../src/models/Section";

use(chaiAsPromised);

describe("QueryHelper", function () {
	let facade: IInsightFacade;

	// Declare datasets used in tests.
	let sections0: string;
	let sections: string;
	let sections1: string;
	let allSections: InsightResult[];

	before(async function () {
		// Just in case there is anything hanging around from a previous run of the test suite
		await clearDisk();

		const timeout = 30000;
		this.timeout(timeout);

		// This block runs once and loads the datasets.
		try {
			sections0 = await getContentFromArchives("miniData7.zip");
			sections = await getContentFromArchives("miniData8.zip");
			sections1 = await getContentFromArchives("pair.zip");
			facade = new InsightFacade();

			const result0 = await facade.addDataset("sections0", sections0, InsightDatasetKind.Sections);
			expect(result0).to.deep.equal(["sections0"]);
			expect(result0).to.be.an("array");
			const result = await facade.addDataset("miniData8", sections, InsightDatasetKind.Sections);
			expect(result).to.deep.equal(["sections0", "miniData8"]);
			expect(result).to.be.an("array");
			const result1 = await facade.addDataset("sections", sections1, InsightDatasetKind.Sections);
			expect(result1).to.deep.equal(["sections0", "miniData8", "sections"]);
			expect(result1).to.be.an("array");

			const query1: Query = {
				WHERE: {},
				OPTIONS: {
					COLUMNS: [
						"sections_dept"
					]
				}
			};
			allSections = await getAllSections(query1, facade.datasetIds);
		} catch (err) {
			throw new Error("Before all hook failed.");
		}
	});

	describe("HandleFilter", function () {

		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		async function checkFilter(this: Mocha.Context): Promise<void> {
			if (!this.test) {
				throw new Error(
					"Invalid call to checkFilter." +
					"Usage: 'checkQuery' must be passed as the second parameter of Mocha's it(..) function." +
					"Do not invoke the function directly."
				);
			}
			// Destructuring assignment to reduce property accesses
			const { input, expected, errorExpected } = await loadTestQuery(this.test.title);
			let result: InsightResult[];
			const query = input as Query;
			try {
				result = handleFilter(query.WHERE, allSections);
				expect(input).to.be.an("object");

				if (errorExpected) {
					expect.fail(`handleFilter resolved when it should have rejected with ${expected}`);
				}
				expect(result).to.deep.equal(expected);
				return;
			} catch (err) {
				if (!errorExpected) {
					expect.fail(`handleFilter threw unexpected error: ${err}`);
				}
				expect(err).to.be.instanceOf(InsightError);
				return;
			}
		}

		// it("[valid/simple.json] SELECT dept, avg WHERE avg > 97", checkFilter);
		// it("[valid/valid1.json] WHERE: OR, GT, LT", checkFilter);
		// it("[valid/wildcardC.json] *wildcard", checkFilter);
		// it("[valid/wildcardC.json] wildcard*", checkFilter);
		// it("[valid/wildcard2astrix.json] wildcard2astrix.json", checkFilter);
		// it("[valid/complexValidQuery.json]", checkFilter);
		it("[valid/negativeAverage(Valid).json]", checkFilter);
		// it("[valid/filterNOT.json]", checkFilter); // passes but takes a long time
		it("[valid/futureYear(Valid).json]", checkFilter);
		// it("[valid/sortOnString.json]", checkFilter);
		// it("[valid/AND.GT.IS.json]", checkFilter);
		// it("[valid/nestedLogicals.json]", checkFilter);
		// it("[valid/wildcardNOT.json]", checkFilter);
		// it("[valid/2Wildcards.json]", checkFilter);
	});

	describe("HandleLogicComparison", function () {

		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		// it("should successfully filter results of complex query containing AND && OR", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[valid/complexValidQuery.json] complex query with AND && OR");
		// 		const query = test.input as Query;
		// 		const result = handleLogicComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleLogicComparison failed: " + error);
		// 	}
		// });

		it("should reject logic that is not AND | OR", async function () {
			try {
				const test = await loadTestQuery("[Filter/invalidLogic.json] query contains XOR");
				const query = test.input as Query;
				handleLogicComparison(query.WHERE, allSections);
				expect.fail("handleMComparison passed when it should have failed");
			} catch (error) {
				expect(error).to.be.instanceOf(InsightError);
			}
		});
	});

	describe("HandleMComparison", function () {

		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		// it("should successfully filter results GT", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[Filter/gtFilter.json] GT 97");
		// 		const query = test.input as Query;
		// 		const result = handleMComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleMComparison failed: " + error);
		// 	}
		// });
		//
		// it("should successfully filter results LT", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[Filter/ltFilter.json] LT 50");
		// 		const query = test.input as Query;
		// 		const result = handleMComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleMComparison failed: " + error);
		// 	}
		// });
		//
		// it("should successfully filter results EQ", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[Filter/eqFilter.json] EQ 69");
		// 		const query = test.input as Query;
		// 		const result = handleMComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleMComparison failed: " + error);
		// 	}
		// });

		it("should reject MComparator that is not LT | GT | EQ", async function () {
			try {
				const test = await loadTestQuery("[Filter/invalidMComparator.json] ORDER's key not found in COLUMN's KEY_LIST array");
				const query = test.input as Query;
				handleMComparison(query.WHERE, allSections);
				expect.fail("handleMComparison passed when it should have failed");
			} catch (error) {
				expect(error).to.be.instanceOf(InsightError);
			}
		});
	});

	describe("HandleSComparison", function () {

		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		// it("should successfully filter results C*", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[valid/Cwildcard.json] C*");
		// 		const query = test.input as Query;
		// 		const result = handleSComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleSComparison failed: " + error);
		// 	}
		// });
		//
		// it("should successfully filter results *C", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[valid/wildcardC.json] *C");
		// 		const query = test.input as Query;
		// 		const result = handleSComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleSComparison failed: " + error);
		// 	}
		// });
		//
		// it("should successfully filter results *C*", async function () {
		// 	try {
		// 		const test = await loadTestQuery("[valid/wildcard2asterix.json] *C*");
		// 		const query = test.input as Query;
		// 		const result = handleSComparison(query.WHERE, allSections);
		// 		expect(result).to.deep.equal(test.expected);
		// 	} catch (error) {
		// 		expect.fail("handleSComparison failed: " + error);
		// 	}
		// });

		it("should reject filtering results C*S", async function () {
			try {
				const test = await loadTestQuery("[invalid/wildCcard.json] C*S");
				const query = test.input as Query;
				handleSComparison(query.WHERE, allSections);
				expect.fail("handleSComparison passed when it should have failed");
			} catch (error) {
				expect(error).to.be.instanceOf(InsightError);
			}
		});
	});

	describe("HandleNegation", function () {

		beforeEach(async function () {
			await clearDisk();
			// const timeout = 10000;
			// this.timeout(timeout);
			// try {
			// 	const answer = 64612;
			// 	const query1: Query = {
			// 		WHERE: {},
			// 		OPTIONS: {
			// 			COLUMNS: [
			// 				"pair_dept"
			// 			]
			// 		}
			// 	};
			// 	allSections = await getAllSections(query1, facade.datasetIds);
			// 	expect(allSections).to.be.an("array");
			// 	expect(allSections.length).to.equal(answer);
			// } catch (e) {
			// 	expect.fail("should not have thrown" + e);
			// }
		});

		afterEach(async function () {
			await clearDisk();
		});

		// it("should successfully filter results given NOT", async function () {
		// 	const timeout = 10000;
		// 	this.timeout(timeout);
		// 	try {
		// 		const test = await loadTestQuery("[Filter/handleNegation.json] negation with nested filters");
		// 		const query = test.input as Query
		// 		const result = handleNegation(query.WHERE as Query, allSections);
		// 		// expect(result).to.have.deep.members(test.expected);
		// 		for (let i=0; i<result.length; i++)  {
		// 			if (!(test.expected).includes(result[i])) {
		// 				console.log(result[i]);
		// 			}
		// 		}
		// 	} catch (error) {
		// 		expect.fail("handleNegation failed: " + error);
		// 	}
		// });
	});

	describe("GetAllSections", function () {
		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("get all sections, given a valid query", async function () {
			const timeout = 10000;
			this.timeout(timeout);
			try {
				const answer = 64612;
				const query1: Query = {
					WHERE: {},
					OPTIONS: {
						COLUMNS: [
							"sections_dept"
						]
					}
				};
				const result = await getAllSections(query1, facade.datasetIds);
				expect(result).to.be.an("array");
				expect(result.length).to.equal(answer);
			} catch (e) {
				expect.fail("should not have thrown" + e);
			}
		});
	});

	describe("sortResults", async function () {
		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("correctly order by column containing strings", async function () {
			try {
				const length = 207;

				const unsorted = await loadTestQuery("[Filter/unsortedString.json] unsorted data");
				const input = unsorted.expected as InsightResult[];

				const sorted = await loadTestQuery("[Filter/sortedString.json] sorted by instructor column");
				const query = sorted.input as Query;
				const insightResult = sorted.expected as InsightResult;

				const result = sortResults(query.OPTIONS, input); // unsorted sm*

				expect(result).to.be.an("array");
				expect(result.length).to.equal(length);
				expect(insightResult).to.have.deep.members(result);
				const validInput = query.OPTIONS;
				if (validInput.ORDER) {
					const field = validInput.ORDER;
					for (let i = 1; i < result.length; i++) {
						if (result[i][field] < result[i - 1][field]) {
							expect.fail("not in correct order");
						}
					}
				}
			} catch (e) {
				expect.fail("Should not have thrown " + e);
			}
		});

		it("correctly order by column containing numbers", async function () {
			try {
				const length = 49;

				const unsorted = await loadTestQuery("[Filter/unsortedNumber.json] unsorted data");
				const input = unsorted.expected as InsightResult[];

				const sorted = await loadTestQuery("[Filter/sortedNumber.json] sorted by avg");
				const query = sorted.input as Query;
				const insightResult = sorted.expected as InsightResult;

				const result = sortResults(query.OPTIONS, input);

				expect(result).to.be.an("array");
				expect(result.length).to.equal(length);
				expect(insightResult).to.have.deep.members(result);
				const validInput = query.OPTIONS;
				if (validInput.ORDER) {
					const field = validInput.ORDER;
					for (let i = 1; i < result.length; i++) {
						if (result[i][field] < result[i - 1][field]) {
							expect.fail("not in correct order");
						}
					}
				}
			} catch (e) {
				expect.fail("Should not have thrown " + e);
			}
		});

		it("should reject sorting when no ORDER in query", async function () {
			try {
				const unsorted = await loadTestQuery("[Filter/unsortedNumber.json] unsorted data");
				const input = unsorted.expected as InsightResult[];
				const query = unsorted.input as Query;

				sortResults(query.OPTIONS, input);
				expect.fail("Should have thrown error");
			} catch (e) {
				expect(e).to.be.instanceOf(InsightError);
			}
		});

		it("should reject sorting when ORDER column contains strings and numbers together", async function () {
			try {
				const unsorted = await loadTestQuery("[Filter/unsortedNumber.json] unsorted data");
				const input: InsightResult[] = [
					{"sections_dept": "EPSE", "sections_avg": 78},
					{"sections_dept": "EPSE", "sections_avg": "99"}
				]
				const query = unsorted.input as Query;

				sortResults(query.OPTIONS, input);
				expect.fail("Should have thrown error");
			} catch (e) {
				expect(e).to.be.instanceOf(InsightError);
			}
		});

		// it("should reject sorting if more than 1 ORDER column", async function () {
		// 	try {
		// 		const unsorted = await loadTestQuery("[Filter/unsortedNumber.json] unsorted data");
		// 		const input: InsightResult[] = [
		// 			{"sections_dept": "EPSE", "sections_avg": 78},
		// 			{"sections_dept": "EPSE", "sections_avg": "99"}
		// 		]
		// 		const query = unsorted.input as Query;
		//
		// 		sortResults(query.OPTIONS, input);
		// 		expect.fail("Should have thrown error");
		// 	} catch (e) {
		// 		expect(e).to.be.instanceOf(InsightError);
		// 	}
		// });
	});

	describe("extractDatasetId", function () {

		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("should successfully extract dataset id from simple query", async function () {
			try {
				const test = await loadTestQuery("[valid/simple.json] simple valid query");
				const result = extractDatasetId(test.input as Query);
				expect(result).to.deep.equal("sections");
			} catch (error) {
				expect.fail("extractDatasetId failed: " + error);
			}
		});

		it("should successfully extract dataset id from complex query", async function () {
			try {
				const test = await loadTestQuery("[valid/complexValidQuery.json] simple valid query");
				const result = extractDatasetId(test.input as Query);
				expect(result).to.deep.equal("sections");
			} catch (error) {
				expect.fail("extractDatasetId failed: " + error);
			}
		});
	});

	describe("LoadDatasets", async function () {
		beforeEach(async function () {
			await clearDisk();
		});

		afterEach(async function () {
			await clearDisk();
		});

		it("successfully load small dataset, given valid id and fileName", async function () {
			try {
				const length = 16;
				const fileName = String(facade.datasetIds.get("sections0"));
				const result = await loadDatasets("sections0", fileName);
				expect(result).to.be.an("array");
				expect(result.length).to.equal(length);
			} catch (e) {
				expect.fail("Should not have thrown " + e);
			}
		});

		it("successfully load large dataset, given valid id and fileName", async function () {
			try {
				const length = 3006;
				const fileName = String(facade.datasetIds.get("miniData8"));
				const result = await loadDatasets("miniData8", fileName);
				expect(result).to.be.an("array");
				expect(result.length).to.equal(length);
			} catch (e) {
				expect.fail("Should not have thrown " + e);
			}
		});

		it("should reject loading dataset if fileName not found", async function () {
			try {
				await loadDatasets("sections", "doesNotExist");
				expect.fail("Should have thrown error");
			} catch (e) {
				expect(e).to.be.instanceOf(InsightError);
			}
		});
	});

});
