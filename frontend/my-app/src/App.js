import {useState} from "react";
import {Alert, Button, Card, Container, Row, Col, Form} from "react-bootstrap";
import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css';

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
    return (
        <Container fluid className="h-100">
            <Row className="h-100">
                <Col md={5} className="d-flex flex-column">
                    <UploadInputFile/>

                </Col>
                <Col md={7} className="d-flex flex-column">
                    <InsightsViewer/>
                </Col>
            </Row>

            <Row>
                <Col>
                    <AddDatasetButton/>
                    <RemoveDatasetButton/>
                    <ViewDatasetButton/>
                </Col>
            </Row>
        </Container>

    )
}

function AddDatasetButton() {
    return (
        <Button className="App-buttons">Add Dataset</Button>
    );
}

function RemoveDatasetButton() {
    return (
        <Button className="App-buttons">Remove Dataset</Button>
    );
}

function ViewDatasetButton() {
    return (
        <Button className="App-buttons">View Dataset</Button>
    );
}

function IdField() {
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
                The ID will be used to uniquely identify your dataset, it may not contain underscores.
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
        if (selectedFile && selectedFile.type === 'application/zip') {
            setFile(selectedFile);
        } else {
            alert('Please upload a valid ZIP file');
        }
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

function UploadInputFile() {
    const [id, setId] = useState('Enter an ID for the dataset');
    const [file, setFile] = useState(null);
    const [error, setError] = useState(null);

    const handleAddDataset = () => {
        if (!id || !file) {
            setError("Please provide a dataset ID and a valid ZIP file")
            return;
        }

        //Handle upload
        console.log("do something with put to upload file");
        alert("dataset added successfully!");
        setError(null);
    };
    return (
        <Container>
            <h2 className="App-header">Add A Dataset</h2>
            <IdField id={id} setId={setId}/>
            <TypeField/>
            <SelectInputFile file={file} setFile={setFile}/>
            <Row className="mt-3">
                <Col className="text-center">
                    <AddDatasetButton onClick={handleAddDataset} />
                </Col>
            </Row>
        </Container>
    )
}

function InsightsViewer() {
    return (
        <Card style={{backgroundColor: '#CD8679', height: "100%"}}>
            <Card.Title className="App-header">
                Insights for a Dataset
            </Card.Title>
            <Card.Body>
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
            </Card.Body>
        </Card>
    )
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