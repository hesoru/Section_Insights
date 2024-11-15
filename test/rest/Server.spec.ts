import { expect } from "chai";
import request, { Response } from "supertest";
import { StatusCodes } from "http-status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import Server from "../../src/rest/Server";
import fs, {readFile} from "fs-extra";
import {clearDisk, getContentFromArchives} from "../TestUtil";

describe("Facade C3", function () {
	const port = 4321;
	let server: Server;
	const SERVER_URL = "http://localhost:4321";
	let ZIP_FILE_SMALL: Buffer;
	let ZIP_FILE_SMALL_2: Buffer;
	let ZIP_FILE_SMALL_3: Buffer;
	let ZIP_FILE_SMALL_4: Buffer;
	let ZIP_FILE_LARGE: Buffer;

	before(async function () {
		await clearDisk();
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
			})

		ZIP_FILE_SMALL = await fs.readFile("C:/Users/Helena/project_team154/test/resources/archives/miniData5.zip");
		ZIP_FILE_SMALL_2 = await fs.readFile("C:/Users/Helena/project_team154/test/resources/archives/miniData6.zip");
		ZIP_FILE_SMALL_3 = await fs.readFile("C:/Users/Helena/project_team154/test/resources/archives/miniData7.zip");
		ZIP_FILE_SMALL_4 = await fs.readFile("C:/Users/Helena/project_team154/test/resources/archives/miniData8.zip");
		ZIP_FILE_LARGE = await fs.readFile("C:/Users/Helena/project_team154/test/resources/archives/pair.zip");
	});

	after(async function () {
		// stop server here once!
		await clearDisk();
		const timeout = 20000;
		this.timeout(timeout);
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

	afterEach(async function () {
		// might want to add some process logging here to keep track of what is going on
		// await clearDisk();
	});

	it("should PUT mini courses dataset on server", async function () {
		const res = await request(SERVER_URL)
			.put("/dataset/miniData5/sections")
			.send(ZIP_FILE_SMALL)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
		// some logging here please!
		// Log.info(`PUT response: ${JSON.stringify(res.body)}`);
		expect(res.body.result).to.include("miniData5");
		expect(res.status).to.be.equal(StatusCodes.OK)
	});

	it("reject PUT request - same mini courses dataset on server twice", async function () {
		const res1 = await request(SERVER_URL)
			.put("/dataset/miniData6/sections")
			.send(ZIP_FILE_SMALL_2)
			.set("Content-Type", "application/x-zip-compressed")
		expect(res1.body.result).to.include("miniData5");
		expect(res1.status).to.be.equal(StatusCodes.OK)

		const res2 = await request(SERVER_URL)
			.put("/dataset/miniData6/sections")
			.send(ZIP_FILE_SMALL_2)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
		// some logging here please!
		// Log.info(`PUT response: ${JSON.stringify(res.body)}`);
		expect(res2.status).to.be.equal(StatusCodes.BAD_REQUEST);
	});

	it("should PUT large courses dataset on server", async function () {
		const timeout = 10000;
		this.timeout(timeout);

		const res = await request(SERVER_URL)
			.put("/dataset/ubc/sections")
			.send(ZIP_FILE_LARGE)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
		// some logging here please!
		// Log.info(`PUT response: ${JSON.stringify(res.body)}`);
		expect(res.body.result).to.include("ubc");
		expect(res.status).to.be.equal(StatusCodes.OK)
	});

	it("should GET list of datasets", async function () {
		const timeout = 10000;
		this.timeout(timeout);

		// await request(SERVER_URL)
		// 	.put("/dataset/miniData7/sections")
		// 	.send(ZIP_FILE_SMALL)
		// 	.set("Content-Type", "application/x-zip-compressed")
		// await request(SERVER_URL)
		// 	.put("/dataset/miniData8/sections")
		// 	.send(ZIP_FILE_SMALL_2)
		// 	.set("Content-Type", "application/x-zip-compressed")

		const res = await request(SERVER_URL)
			.get("/datasets")
		// some logging here please!
		Log.info(`GET response: ${JSON.stringify(res.body)}`)
		expect(res.body.result).to.be.an("array");
		expect(res.body.result).to.include("miniData5");
		expect(res.body.result).to.include("miniData6");
		expect(res.body.result).to.include("ubc");
		expect(res.status).to.be.equal(StatusCodes.OK);
	});

	it("should POST query", async function () {
		const res1 = await request(SERVER_URL)
			.put("/dataset/miniData7/sections")
			.send(ZIP_FILE_SMALL_3)
			.set("Content-Type", "application/x-zip-compressed")
		expect(res1.body.result).to.include("miniData7");
		expect(res1.status).to.be.equal(StatusCodes.OK)

		const query = {
			WHERE: { GT: { "miniData7_avg": 97 } },
			OPTIONS: { COLUMNS: ["miniData7_dept", "miniData7_avg"], ORDER: "miniData7_avg" },
		};

		const res2 = await request(SERVER_URL)
			.post("/query")
			.send(query)
			.set("Content-Type", "application/json")
		// some logging here please!
		Log.info(`POST response: ${JSON.parse(res2.body)}`);
		expect(res2.body.result).to.be.an("array");
		expect(res2.status).to.be.equal(StatusCodes.OK);
	});

	it("should DELETE mini courses dataset from server", async function () {
		await request(SERVER_URL)
			.put("/dataset/miniData8/sections")
			.send(ZIP_FILE_SMALL_4)
			.set("Content-Type", "application/x-zip-compressed")

		const res = await request(SERVER_URL)
			.delete("/dataset/miniData8")
		Log.info(`DELETE response: ${JSON.stringify(res.body)}`);
		expect(res.body.result).to.include("miniData8");
		expect(res.status).to.be.equal(StatusCodes.OK);
	});

	it("reject DELETE request - dataset does not exist", async function () {
		// await request(SERVER_URL)
		// 	.put("/dataset/miniData7/sections")
		// 	.send(ZIP_FILE_SMALL)
		// 	.set("Content-Type", "application/x-zip-compressed")

		const res = await request(SERVER_URL)
			.delete("/dataset/mcgill")
		Log.info(`DELETE response: ${JSON.stringify(res.body)}`);
		expect(res.status).to.be.equal(StatusCodes.NOT_FOUND);
	});

	// TODO: how to create other DELETE error, practically?

	// The other endpoints work similarly. You should be able to find all instructions in the supertest documentation
});
