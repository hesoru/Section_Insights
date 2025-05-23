// import {
// 	InsightError
// } from "../src/controller/IInsightFacade";
// import { clearDisk, loadTestQuery } from "../test/TestUtil";
//
// import { expect, use } from "chai";
// import chaiAsPromised from "chai-as-promised";
// import IInsightFacade from "../src/controller/InsightFacade";
// import { validateBody, validateOptions, validateQuery } from "../src/utils/ValidateHelper";
//
// use(chaiAsPromised);
//
// describe("QueryHelper", function () {
// 	// let facade: IInsightFacade;
//
// 	// Declare datasets used in tests. You should add more datasets like this!
// 	// let sections: string;
//
// 	before(async function () {
// 		// Just in case there is anything hanging around from a previous run of the test suite
// 		await clearDisk();
// 	});
//
// 	describe("validateQuery", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("valid query", async function () {
// 			try {
// 				const test1 = await loadTestQuery("[valid/complexValidQuery.json]");
// 				const test2 = await loadTestQuery("[valid/futureYear(Valid).json]");
// 				const test3 = await loadTestQuery("[valid/inputStringEmpty(Valid).json]");
// 				const test4 = await loadTestQuery("[valid/negativeAverage(Valid).json]");
// 				const test5 = await loadTestQuery("[valid/notFound.json]");
// 				const test6 = await loadTestQuery("[valid/orderMissing(Valid).json]");
// 				const test7 = await loadTestQuery("[valid/rightSyntaxNOT(Valid).json]");
// 				const test8 = await loadTestQuery("[valid/year1900(Valid).json]");
//
// 				validateQuery(test1.input);
// 				validateQuery(test2.input);
// 				validateQuery(test3.input);
// 				validateQuery(test4.input);
// 				validateQuery(test5.input);
// 				validateQuery(test6.input);
// 				validateQuery(test7.input);
// 				validateQuery(test8.input);
// 			} catch (b) {
// 				expect.fail("should not have thrown error" + b);
// 			}
// 		});
//
// 		it("invalid query", async function () {
// 			try {
// 				const test1 = await loadTestQuery("[invalid/columnsMissing.json]");
// 				validateQuery(test1.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			//id in database not tested yet
// 			// try {
// 			//     const test2 = await loadTestQuery('[invalid/idDoesNotExist.json]');
// 			//     validateQuery(test2.input);
// 			//     expect.fail('passed invalidComplex1.json');
// 			// } catch (ea) {
// 			//     expect(ea).to.be.instanceOf(InsightError);
// 			// }
//
// 			try {
// 				const test3 = await loadTestQuery("[invalid/idStringEmpty.json]");
// 				validateQuery(test3.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			//Have not checked wildcards yet
// 			// try {
// 			//     const test4 = await loadTestQuery('[invalid/wildcard(C_S).json]');
// 			//     validateQuery(test4.input);
// 			//     expect.fail('passed invalidComplex1.json');
// 			// } catch (ea) {
// 			//     expect(ea).to.be.instanceOf(InsightError);
// 			// }
//
// 			try {
// 				const test5 = await loadTestQuery("[invalid/invalid.json]");
// 				validateQuery(test5.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			try {
// 				const test6 = await loadTestQuery("[invalid/invalidFilterKeyXOR.json]");
// 				validateQuery(test6.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			try {
// 				const test7 = await loadTestQuery("[invalid/invalidMField.json]");
// 				validateQuery(test7.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			try {
// 				const test8 = await loadTestQuery("[invalid/invalidSField.json]");
// 				validateQuery(test8.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			try {
// 				const test9 = await loadTestQuery("[invalid/mkeyUsedForIS.json]");
// 				validateQuery(test9.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			// try {
// 			// 	const test10 = await loadTestQuery("[invalid/noFilter.json]");
// 			// 	validateQuery(test10.input);
// 			// 	expect.fail("passed invalidComplex1.json");
// 			// } catch (ea) {
// 			// 	expect(ea).to.be.instanceOf(InsightError);
// 			// }
//
// 			try {
// 				const test11 = await loadTestQuery("[invalid/orderKeyMissing.json]");
// 				validateQuery(test11.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			try {
// 				const test12 = await loadTestQuery("[invalid/queryingMultipleDatasets.json]");
// 				validateQuery(test12.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
//
// 			//Havent checked result too large yet
// 			// try {
// 			//     const test13 = await loadTestQuery('[invalid/ResultTooLarge.json]');
// 			//     validateQuery(test13.input);
// 			//     expect.fail('passed invalidComplex1.json');
// 			// } catch (ea) {
// 			//     expect(ea).to.be.instanceOf(InsightError);
// 			// }
//
// 			// try {
// 			// 	const test14 = await loadTestQuery("[invalid/skeyUsedForMcomparator.json]");
// 			// 	validateQuery(test14.input);
// 			// 	expect.fail("passed invalidComplex1.json");
// 			// } catch (ea) {
// 			// 	expect(ea).to.be.instanceOf(InsightError);
// 			// }
//
// 			try {
// 				const test15 = await loadTestQuery("[invalid/wrongSyntaxNOT.json]");
// 				validateQuery(test15.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (ea) {
// 				expect(ea).to.be.instanceOf(InsightError);
// 			}
// 		});
// 	});
//
// 	describe("validateBody", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("testValidBodies", async function () {
// 			try {
// 				const test1 = await loadTestQuery("[Body/validGT.json]");
// 				const test2 = await loadTestQuery("[body/validAND.LT.IS.json]");
// 				const test3 = await loadTestQuery("[body/validIS.json]");
// 				const test4 = await loadTestQuery("[body/validLT.json]");
// 				const test5 = await loadTestQuery("[body/validOR.AND.GT.EQ.json]");
// 				validateBody(test1.input);
// 				validateBody(test2.input);
// 				validateBody(test3.input);
// 				validateBody(test4.input);
// 				validateBody(test5.input);
// 			} catch (e) {
// 				//did not expect error
// 				expect.fail("should not have thrown error" + e);
// 			}
// 		});
//
// 		it("testInvalidBodies", async function () {
// 			try {
// 				const test1 = await loadTestQuery("[body/invalidGT.json]");
// 				validateBody(test1.input);
// 				expect.fail("passed invalidGT.json");
// 			} catch (e) {
// 				expect(e).to.be.instanceOf(InsightError);
// 			}
//
// 			try {
// 				const test2 = await loadTestQuery("[body/invalidComplex1.json]");
// 				validateBody(test2.input);
// 				expect.fail("passed invalidComplex1.json");
// 			} catch (er) {
// 				expect(er).to.be.instanceOf(InsightError);
// 			}
//
// 			// try {
// 			// 	const test3 = await loadTestQuery("[body/invalidComplex2.json]");
// 			// 	validateBody(test3.input);
// 			// 	expect.fail("passed invalidComplex2.json");
// 			// } catch (err) {
// 			// 	expect(err).to.be.instanceOf(InsightError);
// 			// }
//
// 			try {
// 				const test4 = await loadTestQuery("[body/invalidComplex3.json]");
// 				validateBody(test4.input);
// 				expect.fail("passed invalidComplex3.json");
// 			} catch (erro) {
// 				expect(erro).to.be.instanceOf(InsightError);
// 			}
// 		});
// 	});
//
// 	describe("validateOptions", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("validOptions", async function () {
// 			try {
// 				const test1 = await loadTestQuery("[Options/simpleValid.json]");
// 				validateOptions(test1.input);
// 				const test2 = await loadTestQuery("[Options/complexValid.json]");
// 				validateOptions(test2.input);
// 				const test3 = await loadTestQuery("[Options/orderStringValid.json]");
// 				validateOptions(test3.input);
// 				const test4 = await loadTestQuery("[Options/validNoOrder.json]");
// 				validateOptions(test4.input);
// 			} catch (error) {
// 				expect.fail("should not have thrown error" + error);
// 			}
// 		});
//
// 		it("invalidOptions", async function () {
// 			// try {
// 			// 	const test1 = await loadTestQuery("[Options/invalidEmptyColumns.json]");
// 			// 	validateOptions(test1.input);
// 			// 	expect.fail("passed invalidEmptyColumns.json");
// 			// } catch (error) {
// 			// 	expect(error).to.be.instanceOf(InsightError);
// 			// }
// 			try {
// 				const test2 = await loadTestQuery("[Options/invalidNoKey.json]");
// 				validateOptions(test2.input);
// 				expect.fail("passed invalidNoKey.json");
// 			} catch (erroo) {
// 				expect(erroo).to.be.instanceOf(InsightError);
// 			}
// 			try {
// 				const test3 = await loadTestQuery("[Options/invalidOrder.json]");
// 				validateOptions(test3.input);
// 				expect.fail("passed invalidOrder.json");
// 			} catch (esf) {
// 				expect(esf).to.be.instanceOf(InsightError);
// 			}
// 		});
// 	});
//
// 	describe("validateKey", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("valid query", function () {
// 			//readable format of file can be found in src/utils/ANTH312
// 		});
// 	});
//
// 	describe("isMKey", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("valid query", function () {
// 			//readable format of file can be found in src/utils/ANTH312
// 		});
// 	});
// });
