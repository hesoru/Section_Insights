export async function addDataset(id, content, kind) {
    const url = 'http://localhost:<port>'
    try {
        const response = await fetch(`${url}/dataset/:${id}/:${kind}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
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

export async function removeDataset(id) {
    const url = 'http://localhost:<port>'
    try {
        const response = await fetch(`${url}/dataset/:${id}`, {
            method: 'DELETE',
        });

        if(!response.ok) {
            //to make sure the status code is 200 for success.
            const error = await response.json(); //extract the error message
            throw new Error("Error occurred removing dataset from backend" + error);
        }

        //backend fetch was successful
        return await response.json();
    } catch(error) {
        throw new Error("Error removing dataset from backend" + error);
    }
}

export async function performQuery(query) {
    const url = 'http://localhost:<port>'
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