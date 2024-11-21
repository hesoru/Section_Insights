import {useEffect, useState} from "react";
import {Button, Card, Container, Row, Col, Form, Alert, Table} from "react-bootstrap";
import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css';
import {addDatasetAPI, listDatasetsAPI, removeDatasetAPI} from "./Api";
import {generatePieCharts, PieChart} from "./Graph";

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
                    <RemoveDataset/>
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
        <Button onClick={onClick} className="App-buttons">View Dataset</Button>
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
                placeholder="Enter an id for your dataset"
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
                    All sections data should be located in a courses directory and uploaded as a zip file.
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
        if (!id || !file) {
            setMessage("Please provide a dataset ID and a valid ZIP file");
            setAlertType("danger");
            return;
        }
        //Handle upload
        try {
            const kind = "sections";
            await addDatasetAPI(id, file, kind);
            setView("insights");
            setMessage(`Successfully added dataset ${id}!`);
            setAlertType("success");
        } catch (error) {
            setMessage(`Could not add dataset ${id}, please check that you provided a valid dataset id and zip file`);
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
            <IdField id={id} setId={setId} description="The ID will be used to uniquely identify your dataset, it may not contain underscores."/>
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

function RemoveDataset() {
    const [id, setId] = useState("");
    const [message, setMessage] = useState("");
    const [alertType, setAlertType] = useState("");

    const handleRemoveDataset = async () => {
        if (!id) {
            setMessage("Please provide the id of an existing dataset");
            setAlertType("danger");
            return;
        }
        //Handle upload
        try {
            const kind = "Sections";
            await removeDatasetAPI(id, kind);
            setMessage(`Successfully removed dataset ${id}`);
            setAlertType("success");
        } catch (error) {
            setMessage(`Could not remove dataset ${id}, please check that you provided the id of an existing dataset`);
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
            <IdField id={id} setId={setId} description="Enter the ID of the datset you wish to remove."/>
            <Row className="App-buttonrow">
                <Col className="text-center">
                    <RemoveDatasetButton onClick={handleRemoveDataset} />
                </Col>
            </Row>
        </Container>
    )
}

function InsightsViewer({view, setView, id, setId}) {
    return (
        <Card style={{backgroundColor: '#C8D7C1', height: "100%"}}>
            <Card.Title className="App-header">
                {view === "insights" ? "Insights for Dataset id" : "Available Datasets"}
            </Card.Title>
            <Card.Body>
                {view === "insights" ? DisplayInsights({id}) : DisplayDatasets({setId, setView})}
            </Card.Body>
            <Card.Footer>
                <ViewDatasetButton onClick={() => setView("datasets")}/>
            </Card.Footer>
        </Card>
    )
}

function DisplayInsights({id}) {
    return (
        <Card>
            <Row>
                <Col>
                    <Card>
                        <Card.Body>
                            <Card.Title>
                                Graphic 1
                                {id ? (
                                    <PassFailInsight id={id} />
                                ) : (
                                    <p>Pass Fail Insight</p>
                                )}
                            </Card.Title>
                        </Card.Body>
                    </Card>
                </Col>
                <Col>
                    <Card>
                        <Card.Body>
                            <Card.Title>
                                Graphic 2
                            </Card.Title>
                        </Card.Body>
                    </Card>
                </Col>
            </Row>
            <Row className="mt-3">
                <Col>
                    <Card>
                        <Card.Body>
                            <Card.Title>
                                Graphic 3
                            </Card.Title>
                        </Card.Body>
                    </Card>
                </Col>
            </Row>
        </Card>
    )
}

function DisplayDatasets({setId, setView}) {
    const [allDatasets, setAllDatasets] = useState([]);

    useEffect(() => {
        const fetchDatasets = async () => {
            try {
                let allDatasets = await listDatasetsAPI();
                allDatasets = allDatasets.result
                setAllDatasets(allDatasets);
            } catch (error) {
                return <p>Error loading datasets</p>
            }
        };
        fetchDatasets();
    }, []);

    return <DatasetsTable setId={setId} setView={setView} datasets={allDatasets} />;

}

function DatasetsTable({setId, setView, datasets}) {
    if(!datasets || !Array.isArray(datasets)) {
        return <p>No datasets available</p>
    }
    const handleRowClick = (id) => {
        setId(id);
        setView("insights");
    }
    return (
        <Card>
            <Card.Header className="App-tablelabel">Select a dataset from the table below to view it's Insights</Card.Header>
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
                        <h1 className="App-title">Sections Insight</h1>
                        <p className="App-subtitle">Welcome to Sections Insight, a tool providing visual representations
                            of Sections data in the InsightUBC database. Add a dataset to get started!</p>
                        <hr className="my-lg-3" />
                        <p className="App-bannertxt"> By Helena and Sophia</p>
                    </Card.Body>
                </Col>
            </Row>
        </Container>
    )
}

function PassFailInsight({id}) {
    const [allPieData, setAllPieData] = useState(null);
    const [selectedDept, setSelectedDept] = useState(null);
    const [depts, setDepts] = useState(null);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        if(!id){return}
        const fetchPieCharts = async () => {
            try{
                setLoading(true);
                const data = await generatePieCharts({id: id});
                setDepts(data.departments);
                setAllPieData(data.data);
                setLoading(false);
            } catch (error) {
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
        return <p>Loading Pass/Fail data...</p>
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

function DeptAvgInsight() {
    return null;
}

function AvgTrendsInsight() {
    return null;
}