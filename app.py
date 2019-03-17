

import falcon
from falcon_jinja2 import FalconTemplate


falcon_template = FalconTemplate()


class IndexResource:
    @falcon_template.render('index.html')
    def on_get(self, req, resp):
        resp.context = {'framework': 'Falcon'}


app = falcon.API()
app.add_route('/', IndexResource())
