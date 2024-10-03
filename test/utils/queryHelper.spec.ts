import { InsightError } from "../../src/controller/IInsightFacade";
import { clearDisk } from "../TestUtil";

import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";
import {processQueryOnDataset} from "../../src/utils/QueryHelper";

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

    describe("ParseSectionObject", function () {
        beforeEach(async function () {
            await clearDisk();
            //facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid ids with no existing ids", async function () {
            const testQuery = {
                "WHERE":{
                    "GT":{
                        "sections_avg":97
                    }
                }
            }
            try {
                await processQueryOnDataset(testQuery);
            } catch (error) {
                expect.fail("should not thrown an error when adding valid ids" + error);
            }
        });
    });
})
