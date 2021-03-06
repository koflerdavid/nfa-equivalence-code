'use strict';

window.FormUtilities = (function (window) {
    function showErrorMessage(form, message) {
        if (message == null) {
            for (let element of form.getElementsByClassName('generic-error-message')) {
                element.classList.add('visible');
            }
        } else {
            for (let element of form.getElementsByClassName('custom-error-message')) {
                element.classList.add('visible');
                element.innerText = message;
            }
        }
    }

    /**
     * Encodes all key/value pairs of a HTML form into the `application/x-www-form-urlencoded` format.
     * @param form HTMLFormElement
     * @returns {string} `application/x-www-form-urlencoded`-encoded representation of the form.
     */
    function encodeForm(form) {
        const formData = new FormData(form)
            , parts = [];

        for (let key of formData.keys()) {
            let values = formData.getAll(key);

            for (let value of values) {
                const keyValuePair = window.encodeURIComponent(key) + '=' + window.encodeURIComponent(value);
                parts.push(keyValuePair);
            }
        }

        return parts.join('&');
    }

    function hideErrorMessages(form) {
        for (let element of form.getElementsByClassName('error-message')) {
            element.classList.remove('visible');
        }
    }

    function ajaxifyForm(selector, formParser, onSuccess, onError, requestConfigurator) {
        const form = window.document.querySelector(selector)
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

            const encodedFormData = (formParser || encodeForm)(form);

            FormUtilities.hideErrorMessages(form);
            request.send(encodedFormData);

            e.preventDefault();
        });
    }

    function createRequest(url, method) {
        const request = new XMLHttpRequest();
        request.open(method || 'POST', url, true);
        request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        return request;
    }

    return {
        'ajaxifyForm': ajaxifyForm,
        'hideErrorMessages': hideErrorMessages,
        'showErrorMessage': showErrorMessage
    };
})(window);
