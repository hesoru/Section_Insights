// import { clearDisk } from "../test/TestUtil";
//
// import { expect, use } from "chai";
// import chaiAsPromised from "chai-as-promised";
// import { checkValidId } from "../src/utils/JsonHelper";
// import { JSONFile } from "../src/models/Section";
// import {applyNumericOperation} from "../src/utils/TransformationsHelper";
// import {InsightError} from "../src/controller/IInsightFacade";
//
// use(chaiAsPromised);
//
// describe("TransformationHelper", function () {
// 	//let facade: IInsightFacade;
//
// 	// Declare datasets used in tests. You should add more datasets like this!
// 	//let sections: string;
//
// 	before(async function () {
// 		// This block runs once and loads the datasets.
// 		//sections = await getContentFromArchives("pair.zip");
//
// 		// Just in case there is anything hanging around from a previous run of the test suite
// 		await clearDisk();
// 	});
//
// 	describe("groupBy", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
// 	});
//
// 	describe("applyResult", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("valid numeric type", function () {
// 			const group = [
//                 {
//                     "sections_dept": "cnps",
//                     "sections_avg": 99.19
//                 },
//                 {
//                     "sections_dept": "cnps",
//                     "sections_avg": 97.47
//                 },
//                 {
//                     "sections_dept": "cnps",
//                     "sections_avg": 97.47
//                 },
//                 {
//                     "sections_dept": "crwr",
//                     "sections_avg": 98
//                 },
//                 {
//                     "sections_dept": "crwr",
//                     "sections_avg": 98
//                 }];
//             const applyRules = [{applyKey: "sections", applyToken: "MAX", key: "sections_avg"}]
// 			try {
//
// 			} catch (error) {
// 				expect.fail("should not thrown an error when adding valid ids" + error);
// 			}
// 		});
// 	});
//
// 	//also want to check that type being passed to parseSectionObject is always JSONFile
// 	describe("applyNumericOperation", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
// 		it("testingMAX", function () {
// 			const test = [1, 5, 3, 7, 234, 87, 34, 5.75];
//             const token = "MAX";
//             const result = applyNumericOperation(test, token);
//             expect(result).to.equal(234);
//
//             const test1 = [24, 56.1239, 0.324, -24.75, 3];
//             const result1 = applyNumericOperation(test1, token);
//             expect(result1).to.equal(56.1239);
// 		});
//
//         it("testingMIN", function () {
//             const test = [1, 5, 3, 7, 234, 87, 34, 5.75];
//             const token = "MIN";
//             const result = applyNumericOperation(test, token);
//             expect(result).to.equal(1);
//
//             const test1 = [24, 56.1239, 0.324, -24.75, 3];
//             const result1 = applyNumericOperation(test1, token);
//             expect(result1).to.equal(-24.75);
//         });
//
//         it("testingAVG", function () {
//             const test = [1, 5, 3, 7, 234, 87, 34, 5.75];
//             const token = "AVG";
//             const result = applyNumericOperation(test, token);
//             expect(result).to.equal(47.09);
//
//             const test1 = [24, 56.1239, 0.324, -24.75, 3];
//             const result1 = applyNumericOperation(test1, token);
//             expect(result1).to.equal(11.74);
//         });
//
//         it("testingSUM", function () {
//             const test = [1, 5, 3, 7, 234, 87, 34, 5.75];
//             const token = "SUM";
//             const result = applyNumericOperation(test, token);
//             expect(result).to.equal(376.75);
//
//             const test1 = [24, 56.1239, 0.324, -24.75, 3];
//             const result1 = applyNumericOperation(test1, token);
//             expect(result1).to.equal(58.70);
//         });
//
//         it("COUNT", function () {
//             const test = [1, 5, 3, 7, 234, 87, 34, 5.75];
//             const token = "COUNT";
//             const result = applyNumericOperation(test, token);
//             expect(result).to.equal(8);
//
//             const test1 = [24, 56.1239, 0.324];
//             const result1 = applyNumericOperation(test1, token);
//             expect(result1).to.equal(3);
//         });
//
//         it("test invalid", function () {
//             const test = [1, 5, 3, 7, 234, 87, 34, 5.75];
//             const token = "NOTAPPLY";
//             // try {
//             //     applyNumericOperation(test, token);
//             //     expect.fail("should have thrown error");
//             // } catch(error) {
//             //     expect(error).to.be.instanceOf(InsightError);
//             // }
//         });
// 	});
// });
