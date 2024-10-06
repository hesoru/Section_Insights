import { InsightError } from "../../src/controller/IInsightFacade";
import {clearDisk, loadTestQuery} from "../TestUtil";

import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";
import {processQueryOnDataset, validateBody} from "../../src/utils/QueryHelper";

use(chaiAsPromised);

describe("InsightFacade", function () {
    //let facade: IInsightFacade;

    // Declare datasets used in tests. You should add more datasets like this!
    //let sections: string;

    before(async function () {
        // This block runs once and loads the datasets.
        //sections = await getContentFromArchives("pair.zip");

        // Just in case there is anything hanging around from a previous run of the test suite
        await clearDisk();
    });

    describe("validateQuery", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid query", function () {
            //readable format of file can be found in src/utils/ANTH312
        });

    });

    describe("validateBody", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("testValidBodies", async function () {
            try{
                const test = await loadTestQuery('[body/validGT.json]');
                validateBody(test.input);
            } catch (e) {
                //did not expect error
                console.log(e);
            }
        });

    });

    describe("validateOptions", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid query", function () {
            //readable format of file can be found in src/utils/ANTH312
        });

    });

    describe("validateKey", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid query", function () {
            //readable format of file can be found in src/utils/ANTH312
        });

    });

    describe("isMKey", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid query", function () {
            //readable format of file can be found in src/utils/ANTH312
        });

    });

    describe("isSKey", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid query", function () {
            //readable format of file can be found in src/utils/ANTH312
        });

    });
})
