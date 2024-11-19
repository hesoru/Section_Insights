export async function addDatasetAPI(id, content, kind) {
    const url = 'http://localhost:4321'
    try {
        const response = await fetch(`${url}/dataset/${id}/${kind}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/zip',
            },
            //need to convert to base64 encoded
            body: content
        });
        if(!response.ok) {
            //to make sure the status code is 200 for success.
            const error = await response.json(); //extract the error message
            throw new Error("Error occured adding dataset to backend" + error);
        }

        //backend fetch was successful
        return await response.json();
    } catch(error) {
        throw new Error("Error adding dataset to backend" + error);
    }
}

export async function removeDatasetAPI(id) {
    const url = 'http://localhost:4321'
    try {
        const response = await fetch(`${url}/dataset/${id}`, {
            method: 'DELETE',
        });
        if(!response.ok) {
            //to make sure the status code is 200 for success.
            const error = await response.json(); //extract the error message
            throw new Error("Error occurred removing dataset from backend" + error);
        }

        //backend fetch was successful
        console.log("remove4")
        return await response.json();
    } catch(error) {
        throw new Error("Error removing dataset from backend" + error);
    }
}

export async function performQueryAPI(query) {
    const url = 'http://localhost:4321'
    try {
        const response = await fetch(`${url}/${query}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({query})
        });

        if(!response.ok) {
            //to make sure the status code is 200 for success.
            const error = await response.json(); //extract the error message
            throw new Error("Error occurred performing query from backend" + error);
        }

        //backend fetch was successful
        return await response.json();
    } catch(error) {
        throw new Error("Error performing query from backend" + error);
    }
}

export async function listDatasetsAPI() {
    console.log("reached listDatasetAPI")
    const url = 'http://localhost:4321'
    try {
        const response = await fetch(`${url}/datasets`, {
            method: 'GET'
        });

        if(!response.ok) {
            //to make sure the status code is 200 for success.
            const error = await response.json(); //extract the error message
            throw new Error("Error occurred listing datasets from backend" + error);
        }
        console.log("sucessful response from listdatasetAPI")
        //backend fetch was successful
        const jsonData = await response.json()
        console.log(jsonData)
        return jsonData;
    } catch(error) {
        throw new Error("Error listing datasets from backend" + error);
    }
}