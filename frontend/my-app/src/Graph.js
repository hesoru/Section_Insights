import {Bar, Line, Pie} from "react-chartjs-2"
import {Chart as ChartJS, CategoryScale, LinearScale, PointElement, LineElement, Tooltip, Title, Legend, ArcElement, BarElement} from "chart.js"
import {
	getBottomFiveBarChartData,
	getLineGraphData,
	getPieChartData,
	getTopFiveBarChartData
} from "./InsightsHelper";

ChartJS.register(CategoryScale, LinearScale, PointElement, LineElement, Tooltip, Title, Legend, ArcElement, BarElement)
// need labels and datasets
// hoverOffset

export const PieChart = ({data}) => {
    if(!data || data.length === 0) {
        return <div>No data found</div>;
    }
    const options = {responsive: true} // change if trying to make it smaller

    return <Pie options={options} data={data}/>;
}

export const BarChart = ({data}) => {
	if(!data || data.length === 0) {
		return <div>No data found</div>;
	}

	const allData = data.datasets[0].data;
	let min = Math.min(...allData) * 0.9;
	if (min < 0) {
		min = 0;
	}
	let max = Math.max(...allData) * 1.1;
	if (max > 100) {
		max = 100;
	}

	const options = {
		responsive: true,
		plugins: {
			legend: {
				display: false
			}
		},
		scales: {
			x: {
				title: {
					display: true,
					text: 'Department',
				},
			},
			y: {
				title: {
					display: true,
					text: 'Average Grade',
				},
				min: min,
				max: max
			},
		},
	};

	return <Bar options={options} data={data}/>;
}

export const LineGraph = ({data}) => {
	if(!data || data.length === 0) {
		return <div>No data found</div>;
	}
	const options = {
		responsive: true,
		plugins: {
			legend: {
				display: false
			}
		},
		scales: {
			x: {
				title: {
					display: true,
					text: 'Year',
				},
			},
			y: {
				title: {
					display: true,
					text: 'Average Grade',
				},
			},
		},
	};

	return <Line options={options} data={data}/>;
}

export async function generatePieCharts({id, department}) {
    const pieData = (await getPieChartData(id))
    console.log("pieData looking for departments" + JSON.stringify(pieData))
    if (!pieData || pieData.length === 0) {
        return {data: {}, departments: []}
    }
    const departments = [];
    const allPieData = [];

    for (const data of pieData) {
        // console.log("looking at data in pieData" + JSON.stringify(data))
        const department = data.departmentName
        departments.push(department);
        allPieData.push({[department]: data.data});
    }
    return {data: allPieData, departments: departments};
}

export async function generateBottomFiveBarChart({id, department}) {
	const barData = (await getBottomFiveBarChartData(id));
	console.log("barData looking for lowest departments" + JSON.stringify(barData))
	if (!barData || barData.length === 0) {
		return {data: {}}
	}
	return { data: barData.data };
}

export async function generateTopFiveBarChart({id, department}) {
	const barData = (await getTopFiveBarChartData(id));
	console.log("barData looking for highest departments" + JSON.stringify(barData))
	if (!barData || barData.length === 0) {
		return {data: {}}
	}
	return { data: barData.data };
}

export async function generateLineGraphs({id, department}) {
	const lineData = await getLineGraphData(id);
	console.log("lineData looking for departments" + JSON.stringify(lineData))
	if (!lineData || lineData.length === 0) {
		return {data: {}, departments: []}
	}
	const departments = [];
	const allLineData = {};

	for (const data of lineData) {
		// console.log("looking at data in lineData" + JSON.stringify(data))
		const department = data.departmentName
		if (!departments.includes(department)) {
			departments.push(department);
		}
		if (!allLineData[department]) {
			allLineData[department] = data.data;
		}
		// remove duplicate year data
		for (let i = 0; i < data.data.labels.length; i++) {
			const year = data.data.labels[i];
			const grade = data.data.datasets[0].data[i];
			if (!allLineData[department].labels.includes(year)) {
				allLineData[department].labels.push(year);
				allLineData[department].datasets[0].data.push(grade);
			}
		}
	}
	return { data: allLineData, departments };
}
