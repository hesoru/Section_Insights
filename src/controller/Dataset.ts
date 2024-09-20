class Dataset implements InsightDataset {
//can I make the id private and immutable?
//do I need to add these fields here since they are in the interface?
constructor(id: string, kind: InsightDatasetKind, numRows: number) {

this.id = id
this.kind = kind
this.numRows = numRows
}
//Functions
}



class Course {

private rank: number
private sections: [Section] //look into making this fixed length and readOnly

constructor(rank: number, sections: [Section] ) {

this.rank = rank
this.sections = sections
}
//space for functions, getters and setters

}




class Section {

private readonly uuid: string //can not be changed when set, unique identifier for the section
private id: string //course identifier
private title: string
private instructor: string
private dept: string
private year: number
private avg: number
private pass: number
private fail: number

constructor(uuid: string, id: string, title: string, instructor: string, dept: string, year: number, avg: number,
pass: number, fail: number) {

this.course = course
this.title = title
this.instructor = instructor
this.dept = dept
this.year = year
this.avg = avg
this.pass = pass
this.fail = fail
}

//Space for functions
//getters and setters

}

//example instantiation: const LANG401 = new Section(...)
