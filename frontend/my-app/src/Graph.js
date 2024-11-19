import {Bar, Line, Pie} from "react-chartjs-2"
import {Chart as ChartJS, CategoryScale, LinearScale, PointElement, LineElement, Tooltip, Title, Legend, ArcElement} from "chart.js"
import {getPieChartData} from "./InsightsHelper";

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
    const pieData = (await getPieChartData(id))[0]
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
    return {data: allPieData, departments: departments};
}

