window.FormUtilities = (function (document) {
    function showErrorMessage(form, message) {
        if (message == null) {
            byClassName(form, 'generic-error-message')
                .forEach(function (element) {
                    element.classList.add('visible');
                });
        } else {
            byClassName(form, 'custom-error-message')
                .forEach(function (element) {
                    element.classList.add('visible');
                    element.innerText = message;
                });
        }
    }

    function hideErrorMessages(form) {
        byClassName(form, 'error-message')
            .forEach(function (element) {
                element.classList.remove('visible');
            });
    }

    function ajaxifyForm(id, formParser, onSuccess, onError, requestConfigurator) {
        const form = byId(id)
            , action = form.getAttribute('action')
            , method = form.getAttribute('method');

        form.addEventListener('submit', function (e) {
            const request = createRequest(action, method);
            request.addEventListener('load', function () {
                onSuccess(request, form);
            });

            if (requestConfigurator != null) {
                requestConfigurator(request);
            }

            request.addEventListener('error', function (e) {
                onError(e, request, form);
            });

            const encodedFormData = formParser(form);
            request.send(encodedFormData);

            e.preventDefault();
        });
    }

    function byId(id) {
        const element = document.getElementById(id);
        if (element == null) {
            throw new DOMError('Element with id ' + id + ' not found');
        }

        return element;
    }

    function firstByClassName(element, className) {
        const matches = byClassName(element, className);
        if (matches.length > 0) {
            return matches[0];
        }

        throw new DOMError('Element with class ' + className + 'not found');
    }

    function byClassName(element, className) {
        return Array.from(element.getElementsByClassName(className));
    }

    function createRequest(url, method) {
        const request = new XMLHttpRequest();
        request.open(method || 'POST', url, true);
        request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        return request;
    }

    return {
        'ajaxifyForm': ajaxifyForm,
        'byClassName': byClassName,
        'byId': byId,
        'firstByClassName': firstByClassName,
        'hideErrorMessages': hideErrorMessages,
        'showErrorMessage': showErrorMessage
    };
})(window.document);
