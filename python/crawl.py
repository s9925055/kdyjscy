import requests
from pyquery import PyQuery as pq
import json

def search(url, news_id, topic):
    HEADERS = {
        "User-Agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.87 Safari/537.36"
    }
    URL = url
    payload = {}
    resp = requests.get(URL,params=payload,headers = HEADERS)

    if 'error' not in resp.url:
        dollar = pq(resp.content)
        content = dollar('.article-detail p').text()
        forward_num = dollar('.article-info.article-info2 em').text()

        url2 = 'http://huangshengkanjinrong.baijia.baidu.com/ajax/commentlisttieba'
        payload2 = {'url':URL}
        resp2 = requests.get(url2, params=payload2, headers = HEADERS, timeout= 30)
        temps = json.loads(resp2.text)
        if 'comments' in temps:
            comments = temps['data']['comments']
            comment_num = len(comments)
            upvote_num = 0
            if comments is not None:
                for i in range(comment_num):
                    comment = comments[i]['text']
                    vote = comments[i]['support_count']
                    upvote_num += int(vote)
                    # print(comment,news_id,vote)
            search_num = comment_num + upvote_num
        else:
            comment_num = 0
            search_num = 0
            upvote_num = 0
    else:
        content = ""
        forward_num = 0
        comment_num = 0
        search_num = 0
        upvote_num = 0
    return (search_num, comment_num, upvote_num, content, forward_num)


if __name__ == '__main__':
    url = "http://waimai.meituan.com/ajax/comment"
    HEADERS = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36',
               'Host' : 'waimai.meituan.com',
               'Cookie' : 'w_uuid=pik_igkPB1pZpx4mN9EsQU2azlAejTnoPk2P8ihnyrq-x9Fm0lSYqugTXDInG7Ul; _lxsdk_cuid=15e091216c2c8-0de54684de198b-c313760-100200-15e091216c2bc; ci=401; rvct=401%2C10; __utma=211559370.851117358.1503390736.1503390736.1503390736.1; __utmc=211559370; __utmz=211559370.1503390736.1.1.utmcsr=sh.meituan.com|utmccn=(referral)|utmcmd=referral|utmcct=/; __utmv=211559370.|1=city=tb=1; uuid=b72c22c8bbd35c39ff3b.1503390732.0.0.0; oc=FHY6cFzgjK5ieRsHh_XxBY67FdowPCknXCB4KIRyEFPrwFlc6uX04Pm12TOqcO0h3tSMCoWvt8nH7fHBc4ZKS_2ZveeNZrjiOlIOKHT0IdB0DybVn7cj4QwhGccHFVNhYL5f-RuF03cBFBykFKHkH2-aakd3aQKnn2EKECzunJ4; _ga=GA1.2.1722916387.1503390616; _gid=GA1.2.2145800237.1503390616; _gat=1; w_cid=310110; w_cpy_cn="%E6%9D%A8%E6%B5%A6%E5%8C%BA"; w_cpy=yangpuqu; waddrname="%E6%9C%89%E5%AE%B6%E9%85%B8%E8%8F%9C%E9%B1%BC%28%E9%82%AF%E9%83%B8%E8%B7%AF%E5%BA%97%29"; w_geoid=wtw6j4t5sw9v; w_ah="31.303431931883097,121.51585098356009,%E6%9C%89%E5%AE%B6%E9%85%B8%E8%8F%9C%E9%B1%BC%28%E9%82%AF%E9%83%B8%E8%B7%AF%E5%BA%97%29"; JSESSIONID=138802kkybojrdfvbtmqosvf1; _ga=GA1.3.1722916387.1503390616; _gid=GA1.3.2145800237.1503390616; __mta=142465549.1503390618793.1503470195327.1503470203824.16; w_utmz="utm_campaign=baidu&utm_source=1522&utm_medium=(none)&utm_content=(none)&utm_term=(none)"; w_visitid=9f42592a-9d01-4b9c-bf5f-304e06e15784'
               }
    payload = {'wmpoiIdStr': 144897740970367311, 'uuid': 'pik_igkPB1pZpx4mN9EsQU2azlAejTnoPk2P8ihnyrq-x9Fm0lSYqugTXDInG7Ul',
               'originUrl': 'http%3A%2F%2Fwaimai.meituan.com%2Fcomment%2F144897740970367311', 'has_content':1, 'score_grade': 0,
               'platform': 1, 'partner': 4, 'offset': 1, 'has_content': 1, 'score_grade': 0}
    web_content = requests.post(url, headers=HEADERS,  timeout=4)

    print(web_content.raw)
    print(web_content.text)

