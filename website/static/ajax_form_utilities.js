'use strict';

window.FormUtilities = (function (window) {
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
            if (values.length === 0) {
                values = [formData.get(key)];
            }

            for (let value of values) {
                const keyValuePair = window.encodeURIComponent(key) + '=' + window.encodeURIComponent(value);
                parts.push(keyValuePair);
            }
        }

        return parts.join('&');
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

            const encodedFormData = (formParser || encodeForm)(form);
            request.send(encodedFormData);

            e.preventDefault();
        });
    }

    function byId(id) {
        const element = window.document.getElementById(id);
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
})(window);
