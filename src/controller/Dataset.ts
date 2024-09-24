import {InsightDataset, InsightDatasetKind} from "./IInsightFacade";

export default class Dataset implements InsightDataset {

    //can I make the id private and immutable?
    //do I need to add these fields here since they are in the interface?
    private id: string;
    private kind: InsightDatasetKind;
    private numRows: number;

    constructor(id: string, kind: InsightDatasetKind, numRows: number) {
        this.id = id;
        this.kind = kind;
        this.numRows = numRows;
    }


    //Functions
}

export class Course {

    private rank: number;
    private _sections: Section[]; //look into making this fixed length and readOnly

    constructor(rank: number, sections: Section[]) {
        this.rank = rank;
        this._sections = sections;
    }
    //space for functions, getters and setters
    public get sections(): Section[] {
        return this._sections;
    }

    public set sections(value: Section[]) {
        this._sections = value;
    }
}

class Section {
    private readonly uuid: string; //can not be changed when set, unique identifier for the section
    private id: string; //course identifier
    private title: string;
    private instructor: string;
    private dept: string;
    private year: number;
    private avg: number;
    private pass: number;
    private fail: number;

    constructor(
        uuid: string,
        id: string,
        title: string,
        instructor: string,
        dept: string,
        year: number,
        avg: number,
        pass: number,
        fail: number
    ) {
        this.id = id;
        this.uuid = uuid;
        this.title = title;
        this.instructor = instructor;
        this.dept = dept;
        this.year = year;
        this.avg = avg;
        this.pass = pass;
        this.fail = fail;
    }

    //Space for functions
    //getters and setters
}

//Singleton Pattern for commonly accessed data stats like dataset ids
//Singleton Pattern recommended by chatGPT
export class DatabaseInfo {
    private static instance: DatabaseInfo;
    public datasetIds: string[];

    private constructor() {
        this.datasetIds = [];
    }

    public static getInstance(): DatabaseInfo {
        if(!DatabaseInfo.instance) {
            DatabaseInfo.instance = new DatabaseInfo();
        }
        return DatabaseInfo.instance;
    }
}

//example instantiation: const LANG401 = new Section(...)
