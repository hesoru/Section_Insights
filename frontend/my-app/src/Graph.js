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

export async function generatePieCharts({id}) {
    console.log("pie4")
    const pieData = (await getPieChartData(id))[0]
    console.log(typeof pieData)
    console.log(pieData)
    if(!pieData || pieData.length === 0) {
        return <div>No data found</div>;
    }

    const options = {}
    return (
        pieData
        // <div>
        //     {pieData ? (
        //         pieData.map((data, index) => (
        //             <div key={index} className="App-chart">
        //                 <h3>{data.departmentName}</h3>
        //                 <Pie data={data} options={options}/>
        //             </div>
        //         ))
        //     ) : (
        //         <p>Loading chart data...</p>
        //     )}
        //
        // </div>
    );
}

