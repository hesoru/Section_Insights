import {useState} from "react";
import {Button, Card, Container, Row, Col, Form, Alert} from "react-bootstrap";
import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css';
import {addDataset, removeDataset} from "./Api";

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
    const [view, setView] = useState("insights")
    return (
        <Container fluid className="h-100">
            <Row className="h-100">
                <Col md={5} className="d-flex flex-column">
                    <AddDataset setView={setView}/>
                    <hr style={{marginTop: "70px", marginBottom: "30px", border: "1px solid black"}}/>
                    <RemoveDataset/>
                </Col>
                <Col md={7} className="d-flex flex-column">
                    <InsightsViewer view={view} setView={setView}/>
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

function IdField({description}) {
    const [id, setId] = useState('');
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

function AddDataset({setView}) {
    const [id, setId] = useState("");
    const [file, setFile] = useState(null);
    const [message, setMessage] = useState("");
    const [alertType, setAlertType] = useState("");

    const handleAddDataset = async (setView) => {
        if (!id || !file) {
            setMessage("Please provide a dataset ID and a valid ZIP file");
            setAlertType("danger");
            return;
        }
        //Handle upload
        console.log("do something with put to upload file");
        try {
            const kind = "Sections";
            await addDataset(id, file, kind);
            setMessage(`Successfully added dataset ${id}!`);
            setAlertType("success");
            setView("insights");
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
    const [file, setFile] = useState(null);
    const [message, setMessage] = useState("");
    const [alertType, setAlertType] = useState("");

    const handleRemoveDataset = async () => {
        if (!id) {
            setMessage("Please provide the id of an existing dataset");
            setAlertType("danger");
            return;
        }
        //Handle upload
        console.log("do something with put to upload file");
        try {
            const kind = "Sections";
            await removeDataset(id, file, kind);
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

function ViewDataset() {
    const [id, setId] = useState("");
    const [file, setFile] = useState(null);
    const [message, setMessage] = useState("");
    const [alertType, setAlertType] = useState("");

    const handleRemoveDataset = async () => {
        if (!id) {
            setMessage("Please provide the id of an existing dataset");
            setAlertType("danger");
            return;
        }
        //Handle upload
        console.log("do something with put to upload file");
        try {
            const kind = "Sections";
            await removeDataset(id, file, kind);
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

function InsightsViewer({view, setView}) {
    //view can be set to datasets or insights
    return (
        <Card style={{backgroundColor: '#CD8679', height: "100%"}}>
            <Card.Title className="App-header">
                {view === "insights" ? "Insights for Dataset id" : "Available Datasets"}
            </Card.Title>
            <Card.Body>
                {view === "insights" ? DisplayInsights() : DisplayDatasets()}
            </Card.Body>
            <Card.Footer>
                <ViewDatasetButton onClick={() => setView("datasets")}/>
            </Card.Footer>
        </Card>
    )
}

function DisplayInsights() {
    return (
        <Card>
            <Row>
                <Col>
                    <Card>
                        <Card.Body>
                            <Card.Title>
                                Graphic 1
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

function DisplayDatasets() {
}

const Banner = () => {
    return (
        <Container fluid style={{backgroundColor: '#4D6A58'}} className="text-white p-lg-5 text-center">
            <Row>
                <Col>
                    <Card.Body>
                        <h1 className="App-title">Sections Insight</h1>
                        <p className="App-subtitle">A tool providing visual representations of Sections data in the InsightUBC database</p>
                        <hr className="my-lg-3" />
                        <p className="App-bannertxt"> By Helena and Sophia</p>
                    </Card.Body>
                </Col>
            </Row>
        </Container>
    )
}

function PassFailInsight() {
    return null;
}

function DeptAvgInsight() {
    return null;
}

function AvgTrendsInsight() {
    return null;
}