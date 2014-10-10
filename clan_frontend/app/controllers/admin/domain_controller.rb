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
    @vars[:lb_ip] = view_context.content_tag('strong', Rails.configuration.ch_lb.to_s)
  end

  def custom_domain_update
    changed_domain = self.validate_domain(params[:domain])
    unless changed_domain[0,1] =~ /[[:alpha:]]/
      redirect_to(admin_domain_free_path, :alert => t('cp.domain.must_start_with_letter'))
      return
    end
    # TODO: Check uniqueness
    SiteModel.update(session[:user_clan], {:custom_domain => changed_domain})
    redirect_to admin_domain_path, :notice => t('cp.domain.changed_alert')
  end

  def free_domain_edit
    @vars = {}
    @vars[:site] = SiteModel.read(session[:user_clan])
    @vars[:subdomain_base] = Rails.configuration.ch_subdomain_base
  end

  def free_domain_update
    changed_domain = self.validate_domain(params[:domain])
    unless changed_domain[0,1] =~ /[[:alpha:]]/
      redirect_to(admin_domain_free_path, :alert => t('cp.domain.must_start_with_letter'))
      return
    end
    # TODO: Check uniqueness
    SiteModel.update(session[:user_clan], {:free_subdomain => changed_domain})
    redirect_to admin_domain_path, :notice => t('cp.domain.changed_alert')
  end

  def destroy
  end

  def free_subdomain(site)
    free_sd = site['free_subdomain'].present? ?
        site['free_subdomain'] : t('cp.domain.yourclan')
    sub_base = Rails.configuration.ch_subdomain_base
    "#{request.protocol}#{free_sd}.#{sub_base}"
  end

  def custom_domain(site)
    custom_d = site['custom_domain'].present? ?
        site['custom_domain'] : t('cp.domain.customdomain')
    "#{request.protocol}#{custom_d}"
  end

  def validate_domain(d)
    d.truncate(128).
        gsub( /[^A-Za-z0-9\-]+/, '' ).
        gsub(/^-/, '').
        gsub(/-$/, '')
  end
end
