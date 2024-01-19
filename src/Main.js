export const readFileFromFilePickEvent = function(args) {
        return function(onError, onSuccess) {
                const fileInput = args.event.target

                if (!fileInput.files || fileInput.files.length === 0) {
                        alert('No file selected')
                        return onSuccess(args.nothing)
                }

                const selectedFile = fileInput.files[0]
                const fileReader = new FileReader()

                fileReader.onload = function(e) {
                        const fileContents = e.target.result
                        let ui8 = new Uint8Array(fileContents)
                        onSuccess(args.just([...ui8]))
                }

                fileReader.readAsArrayBuffer(selectedFile)

                return function(cancelError, onCancelerError, onCancelerSuccess) {
                        onCancelerSuccess();
                };
        };
};

