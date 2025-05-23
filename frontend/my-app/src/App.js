import {useEffect, useState} from "react";
import {Button, Card, Container, Row, Col, Form, Alert, Table} from "react-bootstrap";
import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css';
import {addDatasetAPI, listDatasetsAPI, removeDatasetAPI} from "./Api";
import {
	BarChart,
	generateBottomFiveBarChart,
	generateLineGraphs,
	generatePieCharts, generateTopFiveBarChart,
	LineGraph,
	PieChart
} from "./Graph";

function App() {
    return (
        <div className="App h-100">
            <Banner/>
            <Layout/>
        </div>
    );
}
export default App;

function Layout() {
    const [view, setView] = useState("datasets")
    const [id, setId] = useState("")
    return (
        <Container fluid className="h-100">
            <Row className="h-100">
                <Col md={5} className="d-flex flex-column">
                    <AddDataset setView={setView} id={id} setId={setId}/>
                    <hr style={{marginTop: "70px", marginBottom: "30px", border: "1px solid black"}}/>
                    <RemoveDataset setView={setView}/>
                </Col>
                <Col md={7} className="d-flex flex-column">
                    <InsightsViewer view={view} setView={setView} id={id} setId={setId}/>
                </Col>
            </Row>
        </Container>
    )
}

function AddDatasetButton({onClick}) {
    return (
        <Button onClick={onClick} className="App-buttons">Add Dataset</Button>
    );
}

function RemoveDatasetButton({onClick}) {
    return (
        <Button onClick={onClick} className="App-buttons">Remove Dataset</Button>
    );
}

function ViewDatasetButton({onClick}) {
    return (
        <Button onClick={onClick} className="App-buttons">View Datasets</Button>
    );
}

function IdField({id, setId, description}) {
    return (
        <Form.Group controlID="id" className="App-fieldtitle">
            <Form.Label className="App-fieldtitle">Dataset ID</Form.Label>
            <Form.Control
                type="text"
                value={id}
                onChange={(e) => setId(e.target.value)}
                placeholder="Enter a dataset ID"
            />
            <Form.Text className="text-muted">
                {description}
            </Form.Text>
        </Form.Group>
    )
}

function TypeField() {
    return (
        <Form.Group controlID="type" className="App-fieldtitle">
            <Form.Label className="App-fieldtitle" >Dataset Type</Form.Label>
            <Form.Control
            type="text"
            value="Sections"
            readOnly
            />
            <Form.Text className="text-muted">
                This website only accepts sections datasets.
            </Form.Text>
        </Form.Group>

    )
}

function SelectInputFile({file, setFile}) {
    const handleFileChange = (e) => {
        const selectedFile = e.target.files[0];
        setFile(selectedFile);
    };

    return (
        <Form>
            <Form.Group controlId="file" className="App-fieldtitle">
                <Form.Label className="App-fieldtitle">Upload Dataset</Form.Label>
            <Form.Control
                type="file"
                accept=".zip"
                onChange={handleFileChange}
            />
                <Form.Text className="text-muted">
                    All sections datasets should be located in a courses directory and uploaded as a zip file.
                </Form.Text>
            </Form.Group>
            {file && <p>Selected File: {file.name}</p>}
        </Form>
    );
}

function AddDataset({setView, id, setId}) {
    const [file, setFile] = useState(null);
    const [message, setMessage] = useState("");
    const [alertType, setAlertType] = useState("");

    const handleAddDataset = async () => {
        //console.log(`reached handleAddDataset ${id}, ${file}`)
        if (!id || !file) {
            //console.log("inside if block")
            setMessage("Please provide a valid dataset ID and a valid dataset zip file.");
            setAlertType("danger");
            return;
        }
        //Handle upload
        //console.log("do something with put to upload file");
        try {
            //console.log("reached inside try block")
            const kind = "sections";
            await addDatasetAPI(id, file, kind);
            //console.log("6")
            setView("insights");
            //console.log("7")
            setMessage(`Successfully added dataset ${id}!`);
            setAlertType("success");
            //console.log("after setting alert success")
        } catch (error) {
            setMessage(`Could not add dataset ${id}, please check that you provided a valid dataset ID and valid dataset zip file.`);
            setAlertType("danger");
        }
    };
    return (
        <Container>
            <h2 className="App-header">Add A Dataset</h2>
            {message && (
                <Alert variant={alertType} onClose={() => setMessage(null)} dismissible>
                    {message}
                </Alert>
            )}
            <IdField id={id} setId={setId} description="This ID will be used to uniquely identify your dataset (it must not contain underscores)."/>
            <TypeField/>
            <SelectInputFile file={file} setFile={setFile}/>
            <Row className="App-buttonrow">
                <Col className="text-center">
                    <AddDatasetButton onClick={handleAddDataset} />
                </Col>
            </Row>
        </Container>
    )
}

function RemoveDataset({setView}) {
    const [id, setId] = useState("");
    const [message, setMessage] = useState("");
    const [alertType, setAlertType] = useState("");

    const handleRemoveDataset = async () => {
        if (!id) {
            setMessage("Please provide the ID of an existing dataset");
            setAlertType("danger");
            return;
        }
        //Handle upload
        //console.log("do something with put to upload file");
        try {
            const kind = "Sections";
            await removeDatasetAPI(id, kind);
            setView("datasets");
            setMessage(`Successfully removed dataset ${id}.`);
            setAlertType("success");
        } catch (error) {
            setMessage(`Could not remove dataset ${id}, please check that you provided the ID of an existing dataset.`);
            setAlertType("danger");
        }
    };
    return (
        <Container>
            <h2 className="App-header">Remove A Dataset</h2>
            {message && (
                <Alert variant={alertType} onClose={() => setMessage(null)} dismissible>
                    {message}
                </Alert>
            )}
            <IdField id={id} setId={setId} description="Enter the ID of the dataset you wish to remove."/>
            <Row className="App-buttonrow">
                <Col className="text-center">
                    <RemoveDatasetButton onClick={handleRemoveDataset} />
                </Col>
            </Row>
        </Container>
    )
}

function InsightsViewer({view, setView, id, setId}) {
    //view can be set to datasets or insights
    //console.log("reached insightsviewer")
    return (
        <Card style={{backgroundColor: '#C8D7C1', height: "100%", borderColor: 'transparent'}}>
            <Card.Title className="App-header">
                {view === "insights" ? "Dataset Insights" : "Available Datasets"}
            </Card.Title>
            <Card.Body>
                <div style={{marginBottom: "70px"}}>
                {view === "insights" ? DisplayInsights({id}) : DisplayDatasets({setId, setView})}
                </div>
                <div>
                <ViewDatasetButton onClick={() => setView("datasets")}/>
                </div>
            </Card.Body>
            <Card.Footer style={{backgroundColor: '#C8D7C1', borderColor: 'transparent'}}>

            </Card.Footer>
        </Card>
    )
}

function DisplayInsights({ id }) {
	return (
		<Card style={{borderColor: 'transparent'}}>

			{/* Department Averages Over Time */}
			<Row className="mt-3">
				<Col>
					<Card style={{borderColor: 'transparent'}}>
						<Card.Body>
							<Card.Title>
								Department Averages Over Time
							</Card.Title>
							{id ? (
								<AvgTrendsInsight id={id} />
							) : (
								<p>Department Averages Over Time Insight</p>
							)}
						</Card.Body>
					</Card>
				</Col>
			</Row>

			<Row>
				{/* Department Averages (Bottom Five and Top Five) */}
				<Col>
					<Card style={{borderColor: 'transparent'}}>
						<Card.Body>
							<Card.Title>
								Bottom Five Department Averages
							</Card.Title>
							{id ? (
								<BottomFiveDeptAvgInsight id={id} />
							) : (
								<p>Bottom Five Department Averages Insight</p>
							)}

							<Card.Title className="mt-3">
								Top Five Department Averages
							</Card.Title>
							{id ? (
								<TopFiveDeptAvgInsight id={id} />
							) : (
								<p>Top Five Department Averages Insight</p>
							)}
						</Card.Body>
					</Card>
				</Col>
			</Row>

			<Row>
				{/* Pass/Fail Ratios */}
				<Col>
					<Card style={{borderColor: 'transparent'}}>
						<Card.Body>
							<Card.Title>
								Student Success in Department Courses
							</Card.Title>
							{id ? (
								<PassFailInsight id={id} />
							) : (
								<p>Pass/Fail Insight</p>
							)}
						</Card.Body>
					</Card>
				</Col>
			</Row>

		</Card>
	);
}

// function DisplayInsights({id}) {
//     return (
//         <Card>
//             <Row>
//                 <Col>
//                     <Card>
//                         <Card.Body>
//                             <Card.Title>
//                                 Department Pass vs. Fail Ratios
// 							</Card.Title>
//                                 {id ? (
//                                     <PassFailInsight id={id} />
//                                 ) : (
//                                     <p>Pass Fail Insight</p>
//                                 )}
//                         </Card.Body>
//                     </Card>
//                 </Col>
//                 <Col>
//                     <Card>
//                         <Card.Body>
// 							<Card.Title>
// 								Department Averages Over Time
// 							</Card.Title>
// 							{id ? (
// 								<AvgTrendsInsight id={id} />
// 							) : (
// 								<p>Average Trends Insight</p>
// 							)}
//                         </Card.Body>
//                     </Card>
//                 </Col>
//             </Row>
// 			<Row>
// 				<Col>
// 					<Card>
// 						<Card.Body>
// 							<Card.Title>
// 								Bottom Five Department Averages
// 							</Card.Title>
// 							{id ? (
// 								<BottomFiveDeptAvgInsight id={id} />
// 							) : (
// 								<p>Bottom Five Department Averages Insight</p>
// 							)}
// 						</Card.Body>
// 					</Card>
// 				</Col>
// 				<Col>
// 					<Card>
// 						<Card.Body>
// 							<Card.Title>
// 								Top Five Department Averages
// 							</Card.Title>
// 							{id ? (
// 								<TopFiveDeptAvgInsight id={id} />
// 							) : (
// 								<p>Top Five Department Averages Insight</p>
// 							)}
// 						</Card.Body>
// 					</Card>
// 				</Col>
// 			</Row>
// 		</Card>
// 	)
// }

function DisplayDatasets({setId, setView}) {
    //console.log("reached display datasets")
    const [allDatasets, setAllDatasets] = useState([]);

    useEffect(() => {
        const fetchDatasets = async () => {
            try {
                let allDatasets = await listDatasetsAPI();
                //console.log("this is what we are looking for!:" + allDatasets);
                allDatasets = allDatasets.result
                //console.log("KEYS" + allDatasets.keys());
                setAllDatasets(allDatasets);
            } catch (error) {
                //console.error("failed to load datasets" + error.message);
                return <p>Error loading datasets</p>
            }
        };
        fetchDatasets();
    }, []);

    return <DatasetsTable setId={setId} setView={setView} datasets={allDatasets} />;

}

function DatasetsTable({setId, setView, datasets}) {
    //console.log("reached DatasetsTable")
    //console.log(typeof datasets);
    //console.log(datasets);
    if(!datasets || !Array.isArray(datasets)) {
        return <p>No datasets available</p>
    }
    const handleRowClick = (id) => {
        setId(id);
        setView("insights");
    }
    return (
        <Card>
            <Card.Header className="App-tablelabel">Select a dataset from the table below to view its Insights</Card.Header>
            <Card.Body>
                <Table striped bordered hover>
                    <tbody>
                    {datasets.map((dataset) => (
                        <tr key={dataset.id} onClick={() => handleRowClick(dataset.id)}>
                            <td>{dataset.id}</td>
                        </tr>
                    ))}
                    </tbody>
                </Table>
            </Card.Body>
        </Card>
    );

}

const Banner = () => {
    return (
        <Container fluid style={{backgroundColor: '#4D6A58'}} className="text-white p-lg-5 text-center">
            <Row>
                <Col>
                    <Card.Body>
                        <h1 className="App-title">Section Insights</h1>
                        <p className="App-subtitle">Welcome to Section Insights: a tool providing visual representations
                            of university course sections datasets. Add a dataset to get started!</p>
                        <hr className="my-lg-3" />
                        <p className="App-bannertxt"> By Helena and Sophia</p>
                    </Card.Body>
                </Col>
            </Row>
        </Container>
    )
}

function PassFailInsight({id}) {
    //console.log("pie2")
    const [allPieData, setAllPieData] = useState(null);
    const [selectedDept, setSelectedDept] = useState(null);
    const [depts, setDepts] = useState(null);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        if(!id){return}
        const fetchPieCharts = async () => {
            //console.log("pie3");
            try{
                setLoading(true);
                //console.log("reached before generatePieCharts")
                const data = await generatePieCharts({id: id});
                // const data = {
                //     data: [{
                //         CHEM: {
                //             labels: ["Pass", "Fail", "Audit"],
                //             datasets: [{
                //                 data: [23, 1, 3],
                //                 backgroundColor: ["blue", "green", "red"],
                //                 hoverOffset: 4
                //             }]
                //         }
                //     },
                //         { BIO: {
                //                 labels: ["Pass", "Fail", "Audit"],
                //                 datasets: [{
                //                     data: [152, 24, 35],
                //                     backgroundColor: ["blue", "green", "red"],
                //                     hoverOffset: 4
                //                 }]
                //             }
                //         }],
                //     departments: ["CHEM", "BIO"]
                // };
                //console.log("reached after generatePieCharts" + JSON.stringify(data))
                setDepts(data.departments);
                setAllPieData(data.data);
                setLoading(false);
                //console.log("pie5")
            } catch (error) {
                //console.error("Error fetching data", error);
                setLoading(false);
            }
        };
        fetchPieCharts();
    }, [id]);

    useEffect(() => {
        if (depts && depts.length > 0 && !selectedDept) {
            setSelectedDept(depts[0]);
        }
    }, [depts, selectedDept]);

    const handleDeptChange = (newDept) => {
        setSelectedDept(newDept);
    }

    if(loading) {
        //console.log("pie load")
        return <p>Loading pass/fail data...</p>
    }
    if(!allPieData) {
        return <p>No data found</p>
    }

    const pieDataForSelectedDept = allPieData.find(
        (data) => data[selectedDept]
    );

    if(!pieDataForSelectedDept) {
        return <p>No data found for {selectedDept}</p>
    }

    return <div>
        <PieChart data={pieDataForSelectedDept[selectedDept]} />

        <select onChange={(e) => handleDeptChange(e.target.value)} value={selectedDept}>
            {depts.map((dept) => (
                <option key={dept} value={dept}>{dept}</option>
                ))}
        </select>
    </div>;
    //return <p>testing</p>
}

function BottomFiveDeptAvgInsight({id}) {
	//console.log("bar")
	const [allBarData, setAllBarData] = useState(null);
	const [loading, setLoading] = useState(false);

	useEffect(() => {
		if(!id){return}
		const fetchBarCharts = async () => {
			//console.log("bar3");
			try{
				setLoading(true);
				//console.log("reached before generateBarCharts")
				const data = await generateBottomFiveBarChart({id: id});
				//console.log("reached after generateBarCharts" + JSON.stringify(data))
				setAllBarData(data.data);
				setLoading(false);
				//console.log("bar5")
			} catch (error) {
				console.error("Error fetching data", error);
				setLoading(false);
			}
		};
		fetchBarCharts();
	}, [id]);

	if(loading) {
		//console.log("bar load")
		return <p>Loading bottom five averages...</p>
	}
	if(!allBarData) {
		return <p>No data found</p>
	}

	return <div>
		<BarChart data={allBarData} />
	</div>;
}

function TopFiveDeptAvgInsight({id}) {
	//console.log("bar")
	const [allBarData, setAllBarData] = useState(null);
	const [loading, setLoading] = useState(false);

	useEffect(() => {
		if(!id){return}
		const fetchBarCharts = async () => {
			//console.log("bar3");
			try{
				setLoading(true);
				//console.log("reached before generateBarCharts")
				const data = await generateTopFiveBarChart({id: id});
				//console.log("reached after generateBarCharts" + JSON.stringify(data))
				setAllBarData(data.data);
				setLoading(false);
				//console.log("bar5")
			} catch (error) {
				console.error("Error fetching data", error);
				setLoading(false);
			}
		};
		fetchBarCharts();
	}, [id]);

	if(loading) {
		//console.log("bar load")
		return <p>Loading top five averages...</p>
	}
	if(!allBarData) {
		return <p>No data found</p>
	}

	return <div>
		<BarChart data={allBarData} />
	</div>;
}

function AvgTrendsInsight({id}) {
	//console.log("line2")
	const [allLineData, setAllLineData] = useState(null);
	const [selectedDept, setSelectedDept] = useState(null);
	const [depts, setDepts] = useState(null);
	const [loading, setLoading] = useState(false);

	useEffect(() => {
		if(!id){return}
		const fetchLineGraphs = async () => {
			//console.log("line3");
			try{
				setLoading(true);
				//console.log("reached before generateLineGraphs")
				const data = await generateLineGraphs({id: id});
				//console.log("reached after generateLineGraphs" + JSON.stringify(data))
				setDepts(data.departments);
				setAllLineData(data.data);
				setLoading(false);
				//console.log("line5")
			} catch (error) {
				console.error("Error fetching data", error);
				setLoading(false);
			}
		};
		fetchLineGraphs();
	}, [id]);

	useEffect(() => {
		if (depts && depts.length > 0 && !selectedDept) {
			setSelectedDept(depts[0]);
		}
	}, [depts, selectedDept]);

	const handleDeptChange = (newDept) => {
		setSelectedDept(newDept);
	}

	if(loading) {
		//console.log("line load")
		return <p>Loading department averages over time...</p>
	}
	if(!allLineData) {
		return <p>No data found</p>
	}

	const lineDataForSelectedDept = allLineData[selectedDept];

	if(!lineDataForSelectedDept) {
		return <p>No data found for {selectedDept}</p>
	}

	return <div>
		<LineGraph data={lineDataForSelectedDept} />

		<select onChange={(e) => handleDeptChange(e.target.value)} value={selectedDept}>
			{depts.map((dept) => (
				<option key={dept} value={dept}>{dept}</option>
			))}
		</select>
	</div>;
}
