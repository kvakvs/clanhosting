class Admin::DomainController < ApplicationController
  def index
    @vars = {}
    @vars[:site] = SiteModel.read(session[:user_clan])
    @vars[:free_subdomain] = free_subdomain(@vars[:site])
    @vars[:custom_domain] = custom_domain(@vars[:site])
    @vars[:clan_index] = view_context.link_to(clan_index_url, clan_index_url)
  end

  def create
  end

  def custom_domain_edit
    @vars = {}
    @vars[:site] = SiteModel.read(session[:user_clan])
  end

  def custom_domain_update
    redirect_to admin_domain_path
  end

  def free_domain_edit
    @vars = {}
    @vars[:site] = SiteModel.read(session[:user_clan])
  end

  def destroy
  end

  def free_subdomain(site)
    free_sd = site['free_subdomain'] || t('cp.domain.yourclan')
    "#{request.protocol}#{free_sd}.clan.#{request.host}"
  end

  def custom_domain(site)
    custom_d = site['custom_domain'] || t('cp.domain.customdomain')
    "#{request.protocol}#{custom_d}"
  end
end
