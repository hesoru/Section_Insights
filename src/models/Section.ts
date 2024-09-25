export interface Section {

     readonly uuid: string; //can not be changed when set, unique identifier for the section
     id: string; //course identifier
     title: string;
     instructor: string;
     dept: string;
     year: number;
     avg: number;
     pass: number;
     fail: number;

}

