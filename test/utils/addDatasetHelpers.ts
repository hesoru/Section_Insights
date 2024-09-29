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
import {checkValidId, parseJSONtoSections, parseSectionObject} from "../../src/utils/JsonHelper";
import {JSONFile, Section} from "../../src/models/Section";

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

    describe("ParseSectionObject", function() {
        beforeEach(async function () {
            await clearDisk();
            facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("this test", async function () {
            try{
                //...
            } catch (error) {
                //...
            }
        });

    });

    describe("CheckValidId", function() {
        beforeEach(async function () {
            await clearDisk();
            facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("valid ids with no existing ids", function () {
            const existingIds: string[] = [];
            try{
                checkValidId('thisIsAValidID', existingIds);
                checkValidId('Also a valid id', existingIds);
                checkValidId('this^@$& to is@#$~^#$&* a valid id', existingIds);
                checkValidId('123325345 3523532563//?><,,,.-=+]', existingIds);

            } catch (error) {
                expect.fail('should not thrown an error when adding valid ids' + error)
            }
        });

        it("valid ids with existing ids", function () {
            const existingIds: string[] = ['alreadyaddeddata', 'also23fds', 'thisissomedata', 'moreids!'];
            try{
                checkValidId('thisIsAValidID', existingIds);
                checkValidId('Another valid one cause why not', existingIds);
                checkValidId('this^@$& to is@23471~`.,=-]a valid id', existingIds);
                checkValidId('123332961023#^$*@(^/?><,,,.-=+]', existingIds);

            } catch (error) {
                expect.fail('should not thrown an error when adding valid ids' + error)
            }
        });

        it("valid ids matching existing ids", function () {
            const existingIds: string[] = ["thisIsAValidID"];
            try{
                checkValidId('thisIsAValidID', existingIds);
                expect.fail('should have thrown error when adding id that already exists in database')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
        });

        it("invalids with no existing ids", function () {
            const existingIds: string[] = [];
            try{
                checkValidId('       ', existingIds);
                expect.fail('should have thrown error when id of only white space')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
            try{
                checkValidId('this_has_underscores', existingIds);
                expect.fail('should have thrown error when id with underscores')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
            try{
                checkValidId('_underscores', existingIds);
                expect.fail('should have thrown error when id with underscores')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
            try{
                checkValidId('underscore_', existingIds);
                expect.fail('should have thrown error when id with underscores')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }

        });

        it("invalid ids with existing ids", function () {
            const existingIds: string[] = ['alreadyaddeddata', 'also23fds', 'thisissomedata', 'moreids!'];
            try{
                checkValidId('       ', existingIds);
                expect.fail('should have thrown error when id of only white space')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
            try{
                checkValidId('this_has_underscores', existingIds);
                expect.fail('should have thrown error when id with underscores')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
            try{
                checkValidId('_underscores', existingIds);
                expect.fail('should have thrown error when id with underscores')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
            try{
                checkValidId('underscore_', existingIds);
                expect.fail('should have thrown error when id with underscores')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
        });

    });

    //also want to check that type being passed to parseSectionObject is always JSONFile
    describe("parseSectionObject", function() {
        beforeEach(async function () {
            await clearDisk();
            facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("parsing valid JSON file", function () {
            const section: JSONFile = {
                Avg: 72.82,
                Course: "312",
                Fail: 1,
                Pass: 75,
                Professor: "bloch, alexia",
                Subject: "anth",
                Title: "intr anth gender",
                Year: 2014,
                id: "12204"
            }

            const expected: Section = {
                    avg: 72.82,
                    id: "312",
                    fail: 1,
                    pass: 75,
                    instructor: "bloch, alexia",
                    dept: "anth",
                    title: "intr anth gender",
                    year: 2014,
                    uuid: "12204"
            }
            try{
                const result = parseSectionObject(section);
                expect(result).deep.equal(expected);
            } catch (error) {
                expect.fail('should not thrown an error parsing valid json section' + error);
            }
        });

        it("parsing invalid JSON file, missing fields", function () {
            const section: JSONFile = <JSONFile>{  //idk if this test is valid
                Avg: 72.82,
                Course: "312",
                Subject: "anth",
                Title: "intr anth gender",
                Year: 2014,
                id: "12204"
            }

            try{
                parseSectionObject(section);
                expect.fail('should have failed when trying to parse JSONSection with missing fields')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
        });

    });

    describe("parseJSONtoSections", function() {
        beforeEach(async function () {
            await clearDisk();
            facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("parsing valid JSON file", function () {
            //readable format of file can be found in src/utils/ANTH312
            const file = '{"result":[{"tier_eighty_five":7,"tier_ninety":2,"Title":"intr anth gender","Section":"001","Detail":"","tier_seventy_two":16,"Other":1,"Low":36,"tier_sixty_four":4,"id":12204,"tier_sixty_eight":8,"tier_zero":0,"tier_seventy_six":16,"tier_thirty":1,"tier_fifty":2,"Professor":"bloch, alexia","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":2,"Year":"2014","tier_twenty":0,"Stddev":10.21,"Enrolled":81,"tier_fifty_five":4,"tier_eighty":8,"tier_sixty":8,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":75,"Fail":1,"Avg":72.82,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":7,"tier_ninety":2,"Title":"intr anth gender","Section":"overall","Detail":"","tier_seventy_two":16,"Other":1,"Low":36,"tier_sixty_four":4,"id":12205,"tier_sixty_eight":8,"tier_zero":0,"tier_seventy_six":16,"tier_thirty":1,"tier_fifty":2,"Professor":"","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":2,"Year":"2014","tier_twenty":0,"Stddev":10.21,"Enrolled":81,"tier_fifty_five":4,"tier_eighty":8,"tier_sixty":8,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":75,"Fail":1,"Avg":72.82,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":3,"tier_ninety":5,"Title":"gender relations","Section":"001","Detail":"a","tier_seventy_two":4,"Other":0,"Low":0,"tier_sixty_four":2,"id":13183,"tier_sixty_eight":4,"tier_zero":1,"tier_seventy_six":8,"tier_thirty":0,"tier_fifty":0,"Professor":"bloch, alexia","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":1,"Year":"2007","tier_twenty":0,"Stddev":15.71,"Enrolled":36,"tier_fifty_five":0,"tier_eighty":6,"tier_sixty":2,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":34,"Fail":1,"Avg":76.03,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":3,"tier_ninety":5,"Title":"gender relations","Section":"overall","Detail":"","tier_seventy_two":4,"Other":0,"Low":0,"tier_sixty_four":2,"id":13184,"tier_sixty_eight":4,"tier_zero":1,"tier_seventy_six":8,"tier_thirty":0,"tier_fifty":0,"Professor":"","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":1,"Year":"2007","tier_twenty":0,"Stddev":15.71,"Enrolled":36,"tier_fifty_five":0,"tier_eighty":6,"tier_sixty":2,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":34,"Fail":1,"Avg":76.03,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":5,"tier_ninety":1,"Title":"intr anth gender","Section":"001","Detail":"","tier_seventy_two":19,"Other":0,"Low":50,"tier_sixty_four":1,"id":25185,"tier_sixty_eight":8,"tier_zero":0,"tier_seventy_six":17,"tier_thirty":0,"tier_fifty":3,"Professor":"thobani, sitara","Audit":0,"tier_g_fifty":0,"tier_forty":0,"Withdrew":4,"Year":"2015","tier_twenty":0,"Stddev":8.02,"Enrolled":84,"tier_fifty_five":2,"tier_eighty":16,"tier_sixty":4,"tier_ten":0,"High":90,"Course":"312","Session":"w","Pass":76,"Fail":0,"Avg":74.7,"Campus":"ubc","Subject":"anth"}],"rank":878}';

            const section1: Section = {
                avg: 72.82,
                id: "312",
                fail: 1,
                pass: 75,
                instructor: "bloch, alexia",
                dept: "anth",
                title: "intr anth gender",
                year: 2014,
                uuid: "12204"
            }

            const section2: Section = {
                avg: 72.82,
                id: "312",
                fail: 1,
                pass: 75,
                instructor: "",
                dept: "anth",
                title: "intr anth gender",
                year: 2014,
                uuid: "12205"
            }

            const section3: Section = {
                avg: 72.03,
                id: "312",
                fail: 1,
                pass: 34,
                instructor: "bloch, alexia",
                dept: "anth",
                title: "gender relations",
                year: 2007,
                uuid: "13183"
            }

            const section4: Section = {
                avg: 76.03,
                id: "312",
                fail: 1,
                pass: 34,
                instructor: "",
                dept: "anth",
                title: "gender relations",
                year: 2007,
                uuid: "13184"
            }

            const section5: Section = {
                avg: 74.7,
                id: "312",
                fail: 0,
                pass: 76,
                instructor: "thobani, sitara",
                dept: "anth",
                title: "intr anth gender",
                year: 2015,
                uuid: "25185"
            }
            const expected: Section[] = [section1, section2, section3, section4, section5];

            try{
                const result = parseJSONtoSections(file);
                expect(result).to.be.an('array');
                expect(result).deep.equal(expected);
            } catch (error) {
                expect.fail('should not thrown an error parsing valid json section' + error);
            }
        });

        it("parsing invalid file, not JSON formatted string", function () {
            const file = "this is not a JSON formatted string"
            const file1 = '{"result":[{"tier_eighty_five":7,"tier_ninety":2,"Title":"intr anth gender","Section":"001","Detail":"","tier_seventy_two":16,"Other":1,"Low":36,"tier_sixty_four":4,"id":12204,"tier_sixty_eight":8,"tier_zero":0,"tier_seventy_six":16,"tier_thirty":1,"tier_fifty":2,"Professor":"bloch, alexia","Audit":0,"tier_g_fifty":1,"tier_forty "tier_seventy_two":16,"Other":1,"Low":36,"tier_sixty_four":4,"id":12205,"tier_sixty_eight":8,"tier_zero":0,"tier_seventy_six":16,"tier_thirty":1,"tier_fifty":2,"Professor":"","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":2,"Year":"2014","tier_twenty":0,"Stddev":10.21,"Enrolled":81,"tier_fifty_five":4,"tier_eighty":8,"tier_sixty":8,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":75,"Fail":1,"Avg":72.82,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":3,"tier_ninety":5,"Title":"gender relations","Section":"001","Detail":"a","tier_seventy_two":4,"Other":0,"Low":0,"tier_sixty_four":2,"id":13183,"tier_sixty_eight":4,"tier_zero":1,"tier_seventy_six"8,"tier_thirty":0,"tier_fifty":0,"Professor":"bloch, alexia","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":1,"Year":"2007","tier_twenty":0,"Stddev":15.71,"Enrolled":36,"tier_fifty_five":0,"tier_eighty":6,"tier_sixty":2,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":34,"Fail":1,"Avg":76.03,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":3,"tier_ninety":5,"Title":"gender relations","Section":"overall","Detail":"","tier_seventy_two":4,"Other":0,"Low":0,"tier_sixty_four":2,"id":13184,"tier_sixty_eight":4,"tier_zero":1,"tier_seventy_six":8,"tier_thirty":0,"tier_fifty":0,"Professor":"","Audit":0,"tier_g_fifty":1,"tier_forty":0,"Withdrew":1,"Year":"2007","tier_twenty":0,"Stddev":15.71,"Enrolled":36,"tier_fifty_five":0,"tier_eighty":6,"tier_sixty":2,"tier_ten":0,"High":94,"Course":"312","Session":"w","Pass":34,"Fail":1,"Avg":76.03,"Campus":"ubc","Subject":"anth"},{"tier_eighty_five":5,"tier_ninety":1,"Title":"intr anth gender","Section":"001","Detail":"","tier_seventy_two":19,"Other":0,"Low":50,"tier_sixty_four":1,"id":25185,"tier_sixty_eight":8,"tier_zero":0,"tier_seventy_six":17,"tier_thirty":0,"tier_fifty":3,"Professor":"thobani, sitara","Audit":0,"tier_g_fifty":0,"tier_forty":0,"Withdrew":4,"Year":"2015","tier_twenty":0,"Stddev":8.02,"Enrolled":84,"tier_fifty_five":2,"tier_eighty":16,"tier_sixty":4,"tier_ten":0,"High":90,"Course":"312","Session":"w","Pass":76,"Fail":0,"Avg":74.7,"Campus":"ubc","Subject":"anth"},"rank":878}';

            try{
                parseJSONtoSections(file);
                expect.fail('should have failed when trying to parse JSONSection with invalid string')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }

            try{
                parseJSONtoSections(file1);
                expect.fail('should have failed when trying to parse JSONSection with invalid string')
            } catch (error) {
                expect(error).to.be.instanceOf(InsightError);
            }
        });
    });

    describe("writeFilesToDisk", function() {
        beforeEach(async function () {
            await clearDisk();
            facade = new InsightFacade();
        });

        afterEach(async function () {
            await clearDisk();
        });

        it("parsing valid JSON file", function () {
            //readable format of file can be found in src/utils/ANTH312

        });
    });
});