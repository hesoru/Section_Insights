export async function addDatasetAPI(id, content, kind) {
    const url = 'http://localhost:4321'
    try {
        console.log("1")
        const response = await fetch(`${url}/dataset/${id}/${kind}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/zip',
            },
            //need to convert to base64 encoded
            body: content
        });
        console.log("2")
        if(!response.ok) {
            console.log("4")
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
    console.log("remove1")
    try {
        const response = await fetch(`${url}/dataset/${id}`, {
            method: 'DELETE',
        });
        console.log("remove2")
        if(!response.ok) {
            console.log("remove3")
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
    console.log(`${url}/${query}`)
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