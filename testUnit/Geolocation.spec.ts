// import { InsightError } from "../src/controller/IInsightFacade";
// import { clearDisk } from "../test/TestUtil";
//
// import { expect, use } from "chai";
// import chaiAsPromised from "chai-as-promised";
// import {getGeolocation} from "../src/utils/GeolocationHelper";
//
// use(chaiAsPromised);
//
// describe("InsightFacade", function () {
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
// 	describe("getGeolocation", function () {
// 		beforeEach(async function () {
// 			await clearDisk();
// 			//facade = new InsightFacade();
// 		});
//
// 		afterEach(async function () {
// 			await clearDisk();
// 		});
//
//         it("test getGeolocation with valid inputs", async function () {
//             const result = await getGeolocation("6245 Agronomy Road V6T 1Z4");
//             const result1 = await getGeolocation("2194 Health Sciences Mall");
//             const result2 = await getGeolocation("6358 University Blvd, V6T 1Z4");
//             const result3 = await getGeolocation("6361 Memorial Road");
//             expect(result).to.deep.equal({lat: 49.26125, lon: -123.24807});
//             expect(result1).to.have.property("lat");
//             expect(result1).to.have.property("lon");
//             expect(result2).to.have.property("lat");
//             expect(result3).to.have.property("lat");
//             expect(result2).to.have.property("lon");
//             expect(result3).to.have.property("lon");
//         })
//
//         it("test getGeolocation with invalid inputs", async function () {
//             try {
//                 await getGeolocation("6245");
//                 expect.fail("should have thrown an error here");
//             } catch (error) {
//                 expect(error).to.be.instanceOf(InsightError);
//             }
//             //how can I trigger the error field of GeoResponse
//         })
//
//
// 	});
// });
