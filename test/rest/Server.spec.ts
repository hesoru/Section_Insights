import { expect } from "chai";
import request, { Response } from "supertest";
import { StatusCodes } from "http-status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import Server from "../../src/rest/Server";
import fs from "fs-extra";
import { clearDisk } from "../TestUtil";

describe("Facade C3", function () {
	const port = 4321;
	let server: Server;
	const SERVER_URL = "http://localhost:4321";
	let ZIP_FILE_SMALL_0: Buffer;
	let ZIP_FILE_SMALL_1: Buffer;
	let ZIP_FILE_SMALL_2: Buffer;
	let ZIP_FILE_SMALL_3: Buffer;
	let ZIP_FILE_SMALL_4: Buffer;
	let ZIP_FILE_LARGE: Buffer;
	let ZIP_FILE_ROOMS_0: Buffer;
	let ZIP_FILE_ROOMS_1: Buffer;

	before(async function () {
		// const timeout = 10000;
		// this.timeout(timeout);

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
			});

		ZIP_FILE_SMALL_0 = await fs.readFile("./test/resources/archives/miniData5.zip");
		ZIP_FILE_SMALL_1 = await fs.readFile("./test/resources/archives/miniData5.zip");
		ZIP_FILE_SMALL_2 = await fs.readFile("./test/resources/archives/miniData5.zip");
		ZIP_FILE_SMALL_3 = await fs.readFile("./test/resources/archives/miniData7.zip");
		ZIP_FILE_SMALL_4 = await fs.readFile("./test/resources/archives/miniData8.zip");
		ZIP_FILE_LARGE = await fs.readFile("./test/resources/archives/pair.zip");
		ZIP_FILE_ROOMS_1 = await fs.readFile("./test/resources/archives/miniCampus1.zip");
		ZIP_FILE_ROOMS_0 = await fs.readFile("./test/resources/archives/campus.zip");
	});

	after(async function () {
		// stop server here once!
		// const timeout = 10000;
		// this.timeout(timeout);

		await clearDisk();
		Log.info("Stopping server after testing");
		await server.stop().then(() => {
			Log.info("Server closed");
		});
	});

	beforeEach(function () {
		// might want to add some process logging here to keep track of what is going on
	});

	afterEach(async function () {
		// might want to add some process logging here to keep track of what is going on
		// await clearDisk();
	});

	it("should PUT mini courses dataset on server (200 response code)", async function () {
		return request(SERVER_URL)
			.put("/dataset/miniData0/sections")
			.send(ZIP_FILE_SMALL_0)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.be.an("array");
				expect(res.body.result).to.include("miniData0");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("second identical PUT request should produce 400 response code", async function () {
		await request(SERVER_URL)
			.put("/dataset/miniData1/sections")
			.send(ZIP_FILE_SMALL_1)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.be.an("array");
				expect(res.body.result).to.include("miniData1");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});

		return request(SERVER_URL)
			.put("/dataset/miniData1/sections")
			.send(ZIP_FILE_SMALL_1)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.status).to.deep.equal(StatusCodes.BAD_REQUEST);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("should PUT mini rooms dataset on server (200 response code)", async function () {
		return request(SERVER_URL)
			.put("/dataset/miniCampus/rooms")
			.send(ZIP_FILE_ROOMS_1)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.be.an("array");
				expect(res.body.result).to.include("miniCampus");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("PUT request with rooms dataset and sections kind should produce 400 response code", async function () {
		return request(SERVER_URL)
			.put("/dataset/miniCampus/sections")
			.send(ZIP_FILE_ROOMS_1)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.status).to.deep.equal(StatusCodes.BAD_REQUEST);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("should PUT large courses dataset on server (200 response code)", async function () {
		const timeout = 10000;
		this.timeout(timeout);

		return request(SERVER_URL)
			.put("/dataset/ubc/sections")
			.send(ZIP_FILE_LARGE)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				// Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("ubc");
				expect(res.status).to.be.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("should GET list of datasets (200 response code)", async function () {
		// const timeout = 10000;
		// this.timeout(timeout);

		await request(SERVER_URL)
			.put("/dataset/miniData2/sections")
			.send(ZIP_FILE_SMALL_2)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("miniData2");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});

		return request(SERVER_URL)
			.get("/datasets")
			.then(function (res: Response) {
				Log.info(`GET response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.be.an("array");
				expect(res.body.result).to.deep.include({
					id: "miniData2",
					kind: "sections",
					numRows: 6,
				});
				expect(res.status).to.be.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	// cannot be done when tests are run asynchronously
	// it("should GET empty list of datasets (200 response code)", async function () {
	// 	return request(SERVER_URL)
	// 		.get("/datasets")
	// 		.then(function (res: Response) {
	// 			Log.info(`GET response: ${JSON.stringify(res.body)}`);
	// 			expect(res.body.result).to.be.an("array");
	// 			expect(res.body.result).to.deep.equal([]);
	// 			expect(res.status).to.be.equal(StatusCodes.OK);
	// 		})
	// 	.catch(function (err: any) {
	// 		Log.info(err.toString());
	// 		expect.fail();
	// 	})
	// });

	it("should POST valid sections query (200 response code)", async function () {
		await request(SERVER_URL)
			.put("/dataset/miniData7/sections")
			.send(ZIP_FILE_SMALL_3)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("miniData7");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});

		const querySections = {
			WHERE: { GT: { miniData7_avg: 80 } },
			OPTIONS: { COLUMNS: ["miniData7_dept", "miniData7_avg"], ORDER: "miniData7_avg" },
		};
		return request(SERVER_URL)
			.post("/query")
			.send(querySections)
			.set("Content-Type", "application/json")
			.then(function (res2: Response) {
				Log.info(`POST response: ${JSON.stringify(res2.body)}`);
				expect(res2.body.result).to.be.an("array");
				expect(res2.body.result).to.deep.equal([
					{
						miniData7_avg: 80.13,
						miniData7_dept: "arth",
					},
					{
						miniData7_avg: 80.13,
						miniData7_dept: "arth",
					},
				]);
				expect(res2.status).to.be.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("should POST valid rooms query (200 response code)", async function () {
		await request(SERVER_URL)
			.put("/dataset/campus/rooms")
			.send(ZIP_FILE_ROOMS_0)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("campus");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});

		const queryRooms = {
			WHERE: {
				AND: [
					{
						IS: {
							campus_furniture: "*Tables*",
						},
					},
					{
						GT: {
							campus_seats: 300,
						},
					},
				],
			},
			OPTIONS: {
				COLUMNS: ["campus_shortname", "maxSeats"],
				ORDER: {
					dir: "DOWN",
					keys: ["maxSeats"],
				},
			},
			TRANSFORMATIONS: {
				GROUP: ["campus_shortname"],
				APPLY: [
					{
						maxSeats: {
							MAX: "campus_seats",
						},
					},
				],
			},
		};
		return request(SERVER_URL)
			.post("/query")
			.send(queryRooms)
			.set("Content-Type", "application/json")
			.then(function (res2: Response) {
				Log.info(`POST response: ${JSON.stringify(res2.body)}`);
				expect(res2.body.result).to.be.an("array");
				expect(res2.body.result).to.deep.equal([
					{
						campus_shortname: "OSBO",
						maxSeats: 442,
					},
					{
						campus_shortname: "HEBB",
						maxSeats: 375,
					},
					{
						campus_shortname: "LSC",
						maxSeats: 350,
					},
				]);
				expect(res2.status).to.be.equal(StatusCodes.OK);
			});
		// .catch(function (err: any) {
		// 	Log.info(err.message);
		// 	expect.fail();
		// });
	});

	it("POST query should produce 400 response code when query has invalid syntax (no COLUMNS)", async function () {
		await request(SERVER_URL)
			.put("/dataset/mcmaster/sections")
			.send(ZIP_FILE_SMALL_0)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("mcmaster");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});

		const query = {
			WHERE: { GT: { mcmaster_avg: 80 } },
			OPTIONS: { ORDER: "mcmaster_avg" },
		};
		return request(SERVER_URL)
			.post("/query")
			.send(query)
			.set("Content-Type", "application/json")
			.then(function (res2: Response) {
				Log.info(`POST response: ${JSON.stringify(res2.body)}`);
				expect(res2.status).to.be.equal(StatusCodes.BAD_REQUEST);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("should DELETE mini courses dataset from server (200 response code)", async function () {
		await request(SERVER_URL)
			.put("/dataset/miniData8/sections")
			.send(ZIP_FILE_SMALL_4)
			.set("Content-Type", "application/x-zip-compressed") // application/x-zip-compressed
			.then(function (res: Response) {
				Log.info(`PUT response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("miniData8");
				expect(res.status).to.deep.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});

		return request(SERVER_URL)
			.delete("/dataset/miniData8")
			.then(function (res: Response) {
				Log.info(`DELETE response: ${JSON.stringify(res.body)}`);
				expect(res.body.result).to.include("miniData8");
				expect(res.status).to.be.equal(StatusCodes.OK);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("DELETE request should produce 400 response code when id contains underscore", async function () {
		return request(SERVER_URL)
			.delete("/dataset/dataset_5")
			.then(function (res: Response) {
				Log.info(`DELETE response: ${JSON.stringify(res.body)}`);
				expect(res.status).to.be.equal(StatusCodes.BAD_REQUEST);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	it("DELETE request should produce 404 response code when dataset does not exist", async function () {
		return request(SERVER_URL)
			.delete("/dataset/mcgill")
			.then(function (res: Response) {
				Log.info(`DELETE response: ${JSON.stringify(res.body)}`);
				expect(res.status).to.be.equal(StatusCodes.NOT_FOUND);
			})
			.catch(function (err: any) {
				Log.info(err.message);
				expect.fail();
			});
	});

	// The other endpoints work similarly. You should be able to find all instructions in the supertest documentation
});
