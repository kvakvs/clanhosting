require 'bertrpc'
Rails.application.bert_svc = BERTRPC::Service.new('localhost', 10000)