import {Bar, Line, Pie} from "react-chartjs-2"
import {Chart as ChartJS, CategoryScale, LinearScale, PointElement, LineElement, Tooltip, Title, Legend, ArcElement} from "chart.js"
import {getPieChartData} from "./InsightsHelper";
import {useState} from "react";

ChartJS.register(CategoryScale, LinearScale, PointElement, LineElement, Tooltip, Title, Legend, ArcElement)
// need labels and datasets.
// hoverOffset

export const PieChart = ({data}) => {
    if(!data || data.length === 0) {
        return <div>No data found</div>;
    }
    const options = {}

    return <Pie options={options} data={data}/>;
}

export async function generatePieCharts({id, department}) {
    console.log("pie4")
    const pieData = (await getPieChartData(id))[0]
    console.log(typeof pieData)
    console.log(pieData)
    if (!pieData || pieData.length === 0) {
        return {data: {}, departments: []}
    }
    const departments = [];
    const allPieData = [];

    for (const data of pieData) {
        departments.push(data.departmentName);
        allPieData[data.departmentName] = {
            labels: data.labels,
            datasets: data.datasets
        };
    }
    //return {data: allPieData, departments: departments};
    return {
        data: [{
            CHEM: {
                labels: ["Pass", "Fail", "Audit"],
                datasets: [{
                    data: [23, 1, 3],
                    backgroundColor: ["blue", "green", "red"],
                    hoverOffset: 4
                }]
            }
        },
            { BIO: {
                labels: ["Pass", "Fail", "Audit"],
                datasets: [{
                    data: [152, 24, 35],
                    backgroundColor: ["blue", "green", "red"],
                    hoverOffset: 4
                }]
            }
            }],
        departments: ["CHEM", "BIO"]
    }
}

