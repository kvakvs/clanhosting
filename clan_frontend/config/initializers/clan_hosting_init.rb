## Имя базового домена например clanhosting.com, или localhost
##
site_addr = 'localhost'
Rails.configuration.ch_domain = site_addr
## Имя базового бесплатного поддомена для пользовательских подсайтов
##
Rails.configuration.ch_subdomain_base = 'clan.' + site_addr
## load balancer IP
##
Rails.configuration.ch_lb = ['127.0.0.1']
