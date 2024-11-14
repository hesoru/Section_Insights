import { expect } from "chai";
import request, { Response } from "supertest";
import { StatusCodes } from "http-status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import {App} from "../../src/App";
import Server from "../../src/rest/Server";

describe("Facade C3", function () {
	const port = 4321;
	let server: Server;
	const SERVER_URL = "http://localhost:4321";

	before(async function () {
		// start server here once and handle errors properly
		Log.info("Starting server for testing");
		server = new Server(port);
		await server
			.start()
			.then(() => {
				Log.info("Server started");
			})
			.catch((err: Error) => {
				Log.error(`Server - ERROR: ${err.message}`);
			});
	});

	after(async function () {
		// stop server here once!
		Log.info("Stopping server after testing");
		await server
			.stop()
			.then(() => {
				Log.info("Server closed");
			})
	});

	beforeEach(function () {
		// might want to add some process logging here to keep track of what is going on
	});

	afterEach(function () {
		// might want to add some process logging here to keep track of what is going on
	});

	// Sample on how to format PUT requests
	it("PUT mini courses dataset on server", async function () {
		const ENDPOINT_URL = "/dataset/miniData7/sections";
		const ZIP_FILE_DATA = "../test/resources/archives/miniData7.zip";

		try {
			return request(SERVER_URL)
				.put(ENDPOINT_URL)
				.send(ZIP_FILE_DATA)
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: Response) {
					// some logging here please!
					Log.info(`PUT response: ${JSON.stringify(res.body)}`);
					expect(res.body.result).to.include("miniData7");
					expect(res.status).to.be.equal(StatusCodes.OK);
				})
				.catch(function (err) {
					// some logging here please!
					Log.error(`PUT /dataset error: ${err.message}`);
					expect.fail();
				});
		} catch (err: any) {
			// and some more logging here!
			Log.error(`PUT /dataset error: ${err.message}`);
			expect.fail();
		}
	});

	it("PUT same mini courses dataset on server twice - reject", async function () {
		const ENDPOINT_URL = "/dataset/miniData7/sections";
		const ZIP_FILE_DATA = "../test/resources/archives/miniData7.zip";

		try {
			return request(SERVER_URL)
				.put(ENDPOINT_URL)
				.send(ZIP_FILE_DATA)
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: Response) {
					// some logging here please!
					expect(res.status).to.be.equal(StatusCodes.BAD_REQUEST);
				})
				.catch(function (err) {
					// some logging here please!
					Log.error(`PUT /dataset error: ${err.message}`);
					expect.fail();
				});
		} catch (err: any) {
			// and some more logging here!
			Log.error(`PUT /dataset error: ${err.message}`);
			expect.fail();
		}
	});

	it("PUT large courses dataset on server", async function () {
		const ENDPOINT_URL = "/dataset/ubc/sections";
		const ZIP_FILE_DATA = "../test/resources/archives/pair.zip";

		try {
			return request(SERVER_URL)
				.put(ENDPOINT_URL)
				.send(ZIP_FILE_DATA)
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: Response) {
					// some logging here please!
					Log.info(`PUT response: ${JSON.stringify(res.body)}`);
					expect(res.body.result).to.include("ubc");
					expect(res.status).to.be.equal(StatusCodes.OK);
				})
				.catch(function (err) {
					// some logging here please!
					Log.error(`PUT /dataset error: ${err.message}`);
					expect.fail();
				});
		} catch (err: any) {
			// and some more logging here!
			Log.error(`PUT /dataset error: ${err.message}`);
			expect.fail();
		}
	});

	it("GET list of datasets", async function () {
		const ENDPOINT_URL = "/datasets";

		try {
			return request(SERVER_URL)
				.get(ENDPOINT_URL)
				.then(function (res: Response) {
					// some logging here please!
					Log.info(`GET response: ${JSON.stringify(res.body)}`);
					expect(res.body.result).to.be.an("array");
					expect(res.status).to.be.equal(StatusCodes.OK);
				})
				.catch(function (err) {
					// some logging here please!
					Log.error(`GET /datasets error: ${err.message}`);
					expect.fail();
				});
		} catch (err: any) {
			// and some more logging here!
			Log.error(`GET /datasets error: ${err.message}`);
			expect.fail();
		}
	});

	it("POST query", async function () {
		const ENDPOINT_URL = "/query";
		const query = {
			WHERE: { GT: { courses_avg: 97 } },
			OPTIONS: { COLUMNS: ["courses_dept", "courses_avg"], ORDER: "courses_avg" },
		};

		try {
			return request(SERVER_URL)
				.post(ENDPOINT_URL)
				.send(query)
				.set("Content-Type", "application/json")
				.then(function (res: Response) {
					// some logging here please!
					Log.info(`POST response: ${JSON.parse(res.body)}`);
					expect(res.body.result).to.be.an("array");
					expect(res.status).to.be.equal(StatusCodes.OK);
				})
				.catch(function (err) {
					// some logging here please!
					Log.error(`POST /query error: ${err.message}`);
					expect.fail();
				});
		} catch (err: any) {
			// and some more logging here!
			Log.error(`POST /query error: ${err.message}`);
			expect.fail();
		}
	});

	// The other endpoints work similarly. You should be able to find all instructions in the supertest documentation
});
